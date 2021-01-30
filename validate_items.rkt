#lang racket

(require "utilities.rkt")

(provide unique-operation-name?
         lone-anonymous-operation?
         field-defined-on-type?
         response-name-conflict?
         leaf-field-scalar?
         nonleaf-field-object?
         argument-defined-on-field?
         unique-argument-name?
         all-arguments-provided?
         inputvalue-coercible?
         unique-fragment-name?
         fragment-target-type-defined-on-schema?
         fragment-target-objecttype?
         fragment-spread-match-typecond?
         fragment-defined-on-document?
         inline-fragment-target-type-defined-on-schema?
         inline-fragment-target-objecttype?
         inline-fragment-match-typecond?
         fragment-spreads-not-form-cycles?)


(define unique-operation-name?
  (lambda (operation names)
    (define name (hash-ref operation 'name))
    (if (or (eq? name 'undefined) (not (hash-has-key? names name)))
        null
        (make-error "There can be only one operation named '~a'." name))))


(define lone-anonymous-operation?
  (lambda (operation opcount)
    (if (and (eq? (hash-ref operation 'name) 'undefined) (< 1 opcount))
        (make-error "This anonymous operation must be the only defined operation.")
        null)))


(define field-defined-on-type?
  (lambda (selection parenttype)
    (define fieldname (hash-ref selection 'name))
    (define fields (hash-ref parenttype 'fields (hash)))  ;;union型はfieldsないからとりあえず (hash)
      
    (if (hash-has-key? fields fieldname)
        null
        (make-error
         "Field '~a' is not defined on type '~a'"
         fieldname (hash-ref parenttype 'name)))))


(define response-name-conflict?
  (lambda (selection names aliases)
    (define alias (hash-ref selection 'alias))
    (define name (hash-ref selection 'name))
      
    (cond
      ((eq? alias 'undefined)
       (if (hash-has-key? aliases name)
           (make-error
             "Fields '~a' conflict because '~a' and '~a' are different fields."
             name (hash-ref aliases name) name)
           null))
      (else 
       (if (hash-has-key? names alias)
           (make-error
             "Fields '~a' conflict because '~a' and '~a' are different fields."
              alias alias name)
           null)))))


(define nonleaf-field-object?
  (lambda (selection parenttype schema)
    (define fieldname (hash-ref selection 'name))
    (define field (hash-ref (hash-ref parenttype 'fields) fieldname))
    (define typename (get-fieldtype-name (hash-ref field 'type)))
      
    (if (or (scalar? typename) (enum? typename schema))
        (make-error
          "Field '~a' must not have a selection since type '~a' has no subfields."
           fieldname typename)
        null)))


(define leaf-field-scalar?
  (lambda (selection parenttype schema)
    (define fieldname (hash-ref selection 'name))
    (define field (hash-ref (hash-ref parenttype 'fields) fieldname))
    (define typename (get-fieldtype-name (hash-ref field 'type)))
      
    (if (or (scalar? typename) (enum? typename schema))
        null
        (make-error
          "Field '~a' of type '~a' must have a selection of subfields."
          fieldname typename))))
              

(define argument-defined-on-field?
  (lambda (argument field parenttype)
    (define argname (hash-ref argument 'name))
    (define argdefs (for/hash ([argdef (hash-ref field 'args)])
                      (values (hash-ref argdef 'name) argdef)))
      
    (if (hash-has-key? argdefs argname)
        null
        (make-error
          "Unknown argument '~a' on field '~a.~a'."
          argname (hash-ref parenttype 'name) (hash-ref field 'name)))))


(define unique-argument-name?
  (lambda (argument names)
    (define argname (hash-ref argument 'name))
    (if (hash-has-key? names argname)
        (make-error "There can be only one argument named '~a'." argname)
        null)))


(define all-arguments-provided?
  (lambda (names field)
    (define errors '())
    (for ([argdef (hash-ref field 'args)])
      (when (and (eq? 'NON_NULL (hash-ref (hash-ref argdef 'type) 'kind))
                 (eq? 'null (hash-ref argdef 'defaultValue))
                 (not (hash-has-key? names (hash-ref argdef 'name))))
        (set! errors
              (append errors
                      (make-error
                       "Field '~a' argument '~a' is required, but it not provided."
                       (hash-ref field 'name) (hash-ref argdef 'name))))))
      errors))


(define literal
  (lambda (value)
    (if (eq? (hash-ref value 'kind) 'LIST)
        (string-append
         "["
         (string-join (map literal (hash-ref value 'value)) ", ")
         "]")
        (format "~s" (hash-ref value 'value)))))

(define inputvalue-coercible?
  (lambda (value type schema)
    (define kind (hash-ref type 'kind))
    (define name (hash-ref type 'name 'undefined))
    (cond
      ((eq? kind 'NON_NULL) (Non_Null-value? value type schema))
      ((eq? kind 'LIST) (List-value? value type schema))
      ((eq? kind 'NULL) null)
      ((eq? name 'Int) (Int-value? value))
      ((eq? name 'Float) (Float-value? value))
      ((eq? name 'String) (String-value? value))
      ((eq? name 'Boolean) (Boolean-value? value))
      ((eq? name 'ID) (ID-value? value))
      (else (Enum-or-Input-coercible? value type schema)))))


(define Non_Null-value?
  (lambda (value type schema)
    (if (eq? (hash-ref value 'value) 'null)
        (make-error "Expected non-null value but found null")
        (inputvalue-coercible? value (hash-ref type 'type) schema))))


(define List-value?
  (lambda (value type schema)
    (define values (hash-ref value 'value))
    (if (list? values)
        (apply append (map (lambda (x) (inputvalue-coercible? x (hash-ref type 'type) schema))
                           values))
        (inputvalue-coercible? value (hash-ref type 'type) schema))))


(define Enum-or-Input-coercible?
  (lambda (value type schema)
    (define typedef (send schema get-type (hash-ref type 'name)))
    (define kind (hash-ref typedef 'kind))
    (cond
      ((eq? kind 'INPUT_OBJECT) (Input-coercible? value typedef schema))
      ((eq? kind 'ENUM) (Enum-coercible? value typedef schema))
      (else (error 'Enum-or-Input-coercible?)))))


(define Input-coercible?
  (lambda (value typedef schema)
    (define names (make-hash))
    (define typename (hash-ref typedef 'name))
    (define fieldsdef (hash-ref typedef 'fields))
    (define errors '())

    (unless (eq? (hash-ref value 'kind) 'OBJECT)
      (set! errors (make-error
                    "Expected value of type '~a', found ~s."
                    typename (hash-ref value 'value))))
    
    (for ([field (hash-ref value 'fields null)])
      (let ([name (hash-ref field 'name)])
        
        (when (hash-has-key? names name)
          (set! errors
                (append errors
                        (make-error
                         "There can be only one input field named \"~a\"."
                         name))))
        
        (if (hash-has-key? fieldsdef name)
            
            (set! errors (append errors
                                 (inputvalue-coercible?
                                  (hash-ref field 'value)
                                  (hash-ref (hash-ref fieldsdef name) 'type)
                                  schema)))
            
            (set! errors (append errors
                                 (make-error
                                  "Field \"~a\" is not defined by type \"~a\""
                                  name typename))))
        
        (hash-set! names (hash-ref field 'name) void)))
     
    (for ([fielddef (hash-values fieldsdef)])
      (when (and (eq? 'NON_NULL (hash-ref (hash-ref fielddef 'type) 'kind))
                 (eq? 'null (hash-ref fielddef 'defaultValue))
                 (not (hash-has-key? names (hash-ref fielddef 'name))))
        
        (set! errors
              (append errors
                      (make-error
                       "Field \"~a.~a\" is required, but it is not provided."
                       typename (hash-ref fielddef 'name))))))
    errors))


(define Enum-coercible?
  (lambda (value typedef schema)
    (define typename (hash-ref typedef 'name))
    (cond
      ((not (eq? (hash-ref value 'kind) 'ENUM))
       (make-error "Enum '~a' cannot represent non-enum value: ~s."
                   typename (literal value)))
      ((not (hash-has-key? (hash-ref typedef 'values) (hash-ref value 'value)))
       (make-error "Value '~a' does not exist in '~a' enum."
                   (literal value) typename))
      (else null))))

  

(define Int-value?
  (lambda (value)
    (if (and (eq? (hash-ref value 'kind) 'INT)
             (< (abs (hash-ref value 'value)) 2147483647))
        null
        (make-error "Int cannot represent non-integer value: ~a" (literal value)))))


(define Float-value?
  (lambda (value)
    (if (number? (hash-ref value 'value))
        null
        (make-error "Float cannot represent non-float value: ~a" (literal value)))))


(define String-value?
  (lambda (value)
    (if (eq? (hash-ref value 'kind) 'STRING)
        null
        (make-error "String cannot represent non-string value: ~a" (literal value)))))


(define Boolean-value?
  (lambda (value)
    (if (eq? (hash-ref value 'kind) 'BOOLEAN)
        null
        (make-error "Boolean cannot represent non-boolean value: ~a" (literal value)))))


(define ID-value?
  (lambda (value)
    (define val (hash-ref value 'value))
    (if (or (string? val) (exact-integer? val))
        null
        (make-error "ID cannot represent non-ID value: ~a" (literal value)))))


(define unique-fragment-name?
  (lambda (fragment names)
    (define name (hash-ref fragment 'name))
    (if (hash-has-key? names name)
        (make-error "There can be only one fragment named '~a'." name)
        null)))


(define fragment-target-type-defined-on-schema?
  (lambda (fragment schema)
    (define typename (hash-ref (hash-ref fragment 'typecond) 'name))
    (cond
      ((scalar? typename) null)
      ((send schema defined-type? typename) null)
      (else (make-error "Unknown type '~a'." typename)))))


(define fragment-target-objecttype?
  (lambda (fragment schema)
    (define name (hash-ref fragment 'name))
    (define typename (hash-ref (hash-ref fragment 'typecond) 'name))
    (define type (send schema get-type typename))
      
    (cond
      ((eq? 'null type) null)
      ((member? (hash-ref type 'kind) (list 'OBJECT 'INTERFACE 'UNION 'undefined)) null)
      (else (make-error
             "Flagment '~a': condition type '~a' is neither ObjectType nor Interface nor UnionType."
             name typename)))))


(define fragment-spread-match-typecond?
  (lambda (fragment-spread parenttype fragments)
    (define parent-typename (hash-ref parenttype 'name))
    (define name (hash-ref fragment-spread 'name))
    (define fragment (hash-ref fragments name #f))
    (define cond-typename (if fragment (hash-ref (hash-ref fragment 'typecond) 'name) #f))
      
    (cond
      ((not fragment) null)
      ((eq? cond-typename parent-typename) null)
      ((and (eq? (hash-ref parenttype 'kind) 'UNION)
            (member? cond-typename (hash-ref parenttype 'types))) null)
      (else (make-error
             "Fragment '~a' cannot be spread here as objects of type '~a' can never be of type '~a'."
             name parent-typename cond-typename)))))


(define fragment-defined-on-document?
  (lambda (fragment-spread fragments)
    (define name (hash-ref fragment-spread 'name))
    (if (hash-has-key? fragments name)
        null
        (make-error "Unknown fragment '~a'." name))))


(define inline-fragment-target-type-defined-on-schema?
  (lambda (inline-fragment schema)
    (fragment-target-type-defined-on-schema? inline-fragment schema)))


(define inline-fragment-target-objecttype?
  (lambda (inline-fragment schema)
    (define typename (hash-ref (hash-ref inline-fragment 'typecond) 'name))
    (define type (send schema get-type typename))
    
    (cond
      ((eq? type 'undefined) null)
      ((member? (hash-ref type 'kind) (list 'OBJECT 'INTERFACE 'UNION)) null)
      (else (make-error "Fragment cannot condition on non composite type '~a'."
                        typename)))))


(define inline-fragment-match-typecond?
  (lambda (inline-fragment parenttype schema)
    (define cond-typename (hash-ref (hash-ref inline-fragment 'typecond) 'name))
    (define parent-typename (hash-ref parenttype 'name))
    
    (cond
      ((eq? cond-typename parent-typename) null)
      ((and (eq? (hash-ref parenttype 'kind) 'UNION) (member? cond-typename (hash-ref parenttype 'types))) null)
      ((send schema defined-type? cond-typename) null)
      (else (make-error
             "Fragment cannot be spread here as objects of type '~a' can never be of type '~a'."
             parent-typename cond-typename)))))


(define fragment-spreads-not-form-cycles?
  (lambda (fragments)
    (define h (for/hash ([fragment fragments])
                (let ([selectionset (hash-ref fragment 'selectionset)])
                  (values (hash-ref fragment 'name)
                          (map (lambda (x) (hash-ref x 'name))
                               (filter (lambda (selection)
                                         (eq? (hash-ref selection 'kind)
                                              'FRAGMENT_SPREAD))
                                       selectionset))))))
      (fsnfc-aux (hash-keys h) h)))


(define fsnfc-aux
  (lambda (ls h)
    (cond
      ((null? ls) null)
      (else
       (let ([l (apply append (map (lambda (x) (hash-ref h x null))
                                   (hash-ref h (car ls))))])

         (if (member? (car ls) l)
             (make-error "fragment spreads cycles start from '~a'" (car l))
             (fsnfc-aux (cdr ls) h)))))))


(define unique-field-name-of-inputtype??
  (lambda (field names)
    (define fieldname (hash-ref field 'name))
    (if (hash-has-key? names fieldname)
        (make-error "There can be only one input field named '~a'." fieldname)
        null)))
