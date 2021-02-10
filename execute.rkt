#lang racket

(require racket/hash)
(require "utilities.rkt"
         "type.rkt"
         "introspection.rkt")

(provide execute)


(define query_cache (make-hash))

(define select-operation
  (lambda (document [operationname #f])
    (define operations
      (for/hash ([op document]
                 #:when (eq? 'OPERATION_DEFINITION (hash-ref op 'kind)))
        (values (hash-ref op 'name) op)))
      
      (cond
        ((not operationname)
         (if (>= 1 (length (hash-keys operations)))
             (hash-ref operations (car (hash-keys operations)))
             (make-error "Must provide operation name if query contains multiple operations.")))
        (else
         (if (hash-has-key? operations operationname)
             (hash-ref operationname)
             (make-error  "Unknown operation named '~a'." operationname))))))


(define get-fragments
  (lambda (document)
    (for/hash ([x document]
               #:when (eq? 'FRAGMENT_DEFINITION (hash-ref x 'kind)))
      (values (hash-ref x 'name) x))))


(define build-exeCtx
  (lambda (schema resolvers document operationname)
    (define operation (select-operation document operationname))
    (define exeCtx (make-hash))
    
    (if (list? operation) ;(= error)
        (hash-set! exeCtx 'errors operation)
        (begin
          (hash-set! exeCtx 'operation operation)
          (hash-set! exeCtx 'fragments (get-fragments document))
          (hash-set! exeCtx 'schema schema)
          (hash-set! exeCtx 'resolvers resolvers)
          (hash-set! exeCtx 'errors '())
          exeCtx))))


(define add-error
  (lambda (exeCtx error)
    (define _errors (hash-ref exeCtx 'errors))
    (hash-set! exeCtx 'errors (append _errors error))))


(define make-response
  (lambda (exeCtx data)
    (define errors (hash-ref exeCtx 'errors))
    
    (if (null? errors)
        (hash 'data data)
        (hash 'data data 'errors errors))))


(define execute
  (lambda (schema resolvers document operationname); [cachekey #f])
    (define exeCtx (build-exeCtx schema resolvers document operationname))
    (define errors (hash-ref exeCtx 'errors))
    (cond
      ((not (null? errors)) errors)
      (else
       (let* ([operation (hash-ref exeCtx 'operation)]
              [data (exec-operation exeCtx operation)])
        (make-response exeCtx data))))))


(define exec-operation
  (lambda (exeCtx operation)
    (define schema (hash-ref exeCtx 'schema))
    (define roottype (send schema get-roottype (hash-ref operation 'operation)))
    (define fields (hash-ref operation 'selectionset))
    
    (exec-fields exeCtx fields roottype (hash))))


(define spread-fragment
  (lambda (exeCtx fragment_spread type)
    (define fragments (hash-ref exeCtx 'fragments))
    (define fragment (hash-ref fragments (hash-ref fragment_spread 'name)))
    
    (if (eq? (hash-ref type 'name)
             (hash-ref (hash-ref fragment 'typecond) 'name))
        (hash-ref fragment 'selectionset)
        '())))


(define spread-inlinefragment
  (lambda (exeCtx inline_fragment type)
    (if (eq? (hash-ref type 'name)
             (hash-ref (hash-ref inline_fragment 'typecond) 'name))
        (hash-ref inline_fragment 'selectionset)
        '())))


(define exec-fields
  (lambda (exeCtx fields parenttype data [result (hash)])
    (define field (if (null? fields) '() (car fields)))
    (cond
      ((null? fields) result)
      
      ((eq? (hash-ref field 'kind) 'FRAGMENT_SPREAD)
       (let ([_fields (spread-fragment exeCtx field parenttype)])
         (exec-fields exeCtx (append _fields (cdr fields)) parenttype data result)))
      
      ((eq? (hash-ref field 'kind) 'INLINE_FRAGMENT)
       (let ([_fields (spread-inlinefragment exeCtx field parenttype)])
         (exec-fields exeCtx (append _fields (cdr fields)) parenttype data result)))
      
      (else
       (let ([x (exec-field exeCtx field parenttype data)]
             [name (if (eq? 'undefined (hash-ref field 'alias))
                       (hash-ref field 'name)
                       (hash-ref field 'alias))])
         (if (eq? x 'error)
             'null
             (exec-fields exeCtx (cdr fields) parenttype data (hash-set result name x))))))))


(define default-resolver
  (lambda (parent args info)
    (if (hash? parent)
        (hash-ref parent (hash-ref info 'fieldname) 'null)
        'null)))


(define select-resolver
  (lambda (exeCtx type field)
    (define resolvers (hash-ref exeCtx 'resolvers))
    (define typename (hash-ref type 'name))
    (define fieldname (hash-ref field 'name))
    (define fieldresolvers (hash-ref resolvers typename (hash)))
    (define i-fieldresolvers (hash-ref introspection-resolvers
                                       typename (hash)))
    (cond    
      ((hash-has-key? fieldresolvers fieldname)
       (hash-ref fieldresolvers fieldname))
      
      ((eq? '__typename fieldname) __typename-resolver)
      ((eq? '__schema fieldname) __schema-resolver)
      ((eq? '__type fieldname) __type-resolver)
      ((hash-has-key? i-fieldresolvers fieldname)
       (hash-ref i-fieldresolvers fieldname))
      (else default-resolver))))


(define complete-argvalues
  (lambda (argdefs argsmap schema)
    (define args (make-hash))
    (for ([argdef argdefs])
      (let* ([name (hash-ref argdef 'name)]
             [defaultValue (hash-ref argdef 'defaultValue)]
             [inputValue (hash-ref (hash-ref argsmap name (hash)) 'value 'undefined)])
      (cond
        ((eq? inputValue 'undefined)
         (unless (eq? defaultValue 'null)
           (hash-set! args name defaultValue)))
        (else
         (hash-set! args name
                    (coerce-input-value inputValue (hash-ref argdef 'type) schema))))))
    args))



(define make-resolver-args
  (lambda (fielddef field schema)
    (define argdefs (hash-ref fielddef 'args))
    (define argsmap (for/hash ([arg (hash-ref field 'args)])
                      (values (hash-ref arg 'name) arg)))

    (complete-argvalues argdefs argsmap schema)))
          
            
(define make-resolver-info
  (lambda (exeCtx field parenttype)
    (hash 'schema (hash-ref exeCtx 'schema)
          'fieldname (hash-ref field 'name)
          'returntype (hash-ref field 'type)
          'parenttype (hash-ref parenttype 'name))))


(define get-fielddefinition
  (lambda (type fieldname)
    (cond
      ((eq? fieldname '__typename) __typename-fielddef)
      ((eq? fieldname '__schema) __schema-fielddef)
      ((eq? fieldname '__type) __type-fielddef)
      (else
       (hash-ref (hash-ref type 'fields) fieldname)))))


(define exec-field
  (lambda (exeCtx field parenttype data)
    (define fielddef (get-fielddefinition parenttype (hash-ref field 'name)))
    (define args (make-resolver-args fielddef field (hash-ref exeCtx 'schema)))
    (define info (make-resolver-info exeCtx fielddef parenttype))
    (define res_type (hash-ref fielddef 'type))  
    (define resolver (select-resolver exeCtx parenttype field))
    
    (define result (if (procedure? resolver)
                       (resolver data args info)
                       resolver))
     
    (complete-field exeCtx field res_type parenttype result info)))


(define complete-field
  (lambda (exeCtx field res_type parenttype data info) 
    (cond
      ((nonnull-type? res_type) (complete-nonnull-field exeCtx field res_type parenttype data info))
      ((eq? data 'null) 'null)
      ((list-type? res_type) (complete-list-field exeCtx field res_type parenttype data info))
      ((scalar-type? res_type) (coerce-output-scaler data res_type))
      ((named-type? res_type) (complete-named-field exeCtx field res_type data info))
      (else (error 'complete-field)))))


(define complete-nonnull-field
  (lambda (exeCtx field res_type parenttype data info)
    (define _data (complete-field  exeCtx field (hash-ref res_type 'type) parenttype data info))
    (if (eq? _data 'null)
        (begin
          (add-error exeCtx
                     (make-error "Cannot return null for non-nullable field ~a.~a."
                                 (hash-ref parenttype 'name) (hash-ref field 'name)))
          'error)
        _data)))


(define complete-list-field
  (lambda (exeCtx field res_type parenttype data info [result '()])
    (cond
      ((eq? data 'null) 'null)
      ((null? data) result)
      ((list? data)
       (let ([comp-item (complete-field exeCtx field (hash-ref res_type 'type) parenttype (car data) info)])
         (if (eq? comp-item 'error)
             'error
             (complete-list-field exeCtx field res_type parenttype (cdr data) info
                                  (append result (list comp-item))))))
      (else
       (begin
         (add-error exeCtx
                    (make-error "Expected Iterable, but did not find one for field ~a.~a."
                                 (hash-ref parenttype 'name) (hash-ref field 'name)))
         'error)))))

       
(define get-type-kind
  (lambda (exeCtx typename)
    (define schema (hash-ref exeCtx 'schema))
    (define type (send schema get-type typename))
    (hash-ref type 'kind)))


(define complete-named-field
  (lambda (exeCtx field res_type data info)
    (define type-kind (get-type-kind exeCtx (hash-ref res_type 'name)))
    (cond
      ((eq? type-kind 'OBJECT) (complete-object-field exeCtx field res_type data))
      ((eq? type-kind 'ENUM) (complete-enum-field exeCtx field res_type data info))
      ((eq? type-kind 'UNION) (complete-union-field exeCtx field res_type data info))
      (else (error 'complete-named-field)))))


(define complete-enum-field
  (lambda (exeCtx field res_type data info)
    (define typename (hash-ref res_type 'name))
    (define schema (hash-ref exeCtx 'schema))
    (define type (send schema get-type typename))
    (define enumvalues (hash-ref type 'values))

    (cond
      ((and (string? data) (hash-has-key? enumvalues data))
       data)
      ((and (symbol? data) (hash-has-key? enumvalues (symbol->string data)))
       (symbol->string data))
      (else
       (begin
         (add-error exeCtx
                    (make-error "Enum '~a' cannot represent value: ~a"
                                typename data))
         'error)))))


(define complete-object-field
  (lambda (exeCtx field res_type data)
    (define fields (hash-ref field 'selectionset))
    (define typename (hash-ref res_type 'name))
    (define type (send (hash-ref exeCtx 'schema) get-type typename))
    
    (exec-fields exeCtx fields type data)))


(define complete-union-field
  (lambda (exeCtx field res_type data info)
    (define resolvers (hash-ref exeCtx 'resolvers))
    (define typename (hash-ref res_type 'name))
    (define resolver (hash-ref (hash-ref resolvers typename) '__resolveType))
    (define resolveType (resolver data (hash) info))
    (define type (send (hash-ref exeCtx 'schema) get-type resolveType))
    
    (complete-object-field exeCtx field type data)))


