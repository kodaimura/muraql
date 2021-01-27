#lang racket

(require "utilities.rkt"
         "sdlparse.rkt"
         "introspection.rkt"
         "schemavalidate.rkt")

(provide build-schema)


(define build-schema
  (lambda (typedefs resolvers)
    (define typesAst (if (string? typedefs)
                         (sdlparse typedefs)
                         typedefs))
    (validate-schema typesAst)
    (validate-resolvers typesAst resolvers)
    (build-schema-from-typesAst typesAst)))


(define build-schema-from-typesAst
  (lambda (typesAst)
    (set! typesAst (append typesAst introspection-typedefs))

    (define schemaobj (get-schema-from-typesAst typesAst))
    (define typedefs (get-typedefs-from-typesAst typesAst))

    (define querytype #f)
    (define mutationtype #f)
    (define subscriptiontype #f)
    
    (when schemaobj
      (for ([field (hash-ref schemaobj 'fields)])
        (let* ([operation (hash-ref field 'operation)]
               [typename (hash-ref field 'typename)]
               [typedef (pick-typedef typedefs typename)])
          (cond
            ((eq? operation 'query) (set! querytype typedef))
            ((eq? operation 'mutation) (set! mutationtype typedef))
            ((eq? operation 'subscription) (set! subscriptiontype typedef))
            (else (error 'build-schema-from-typesAst))))))
    
    (when (not schemaobj)
      (let ([qutype (pick-typedef typedefs 'Query)]
            [mutype (pick-typedef typedefs 'Mutation)]
            [sutype (pick-typedef typedefs 'Subscription)])
        (set! querytype qutype)
        (when mutype (set! mutationtype mutype))
        (when sutype (set! subscriptiontype sutype))))

    (new Schema%
         [typedefs typedefs]
         [querytype querytype]
         [mutationtype mutationtype]
         [subscriptiontype subscriptiontype])))


(define pick-typedef
  (lambda (typedefs typename)
    (cond
      ((null? typedefs) #f)
      ((eq? (hash-ref (car typedefs) 'name) typename) (car typedefs))
      (else (pick-typedef (cdr typedefs) typename)))))


;;schemaオブジェクト定義の部分だけをとる
(define get-schema-from-typesAst
  (lambda (typesAst)
    (findf (lambda (x) (eq? (hash-ref x 'kind) 'schema))
           typesAst)))

;;型の定義部分をとる
(define get-typedefs-from-typesAst
  (lambda (typesAst)
    (filter-not (lambda (x) (eq? (hash-ref x 'kind) 'schema))
                typesAst)))



(define typedef->fieldsmap
  (lambda (typedef)
    (define fields (hash-ref typedef 'fields))
    (define fieldsmap (for/hash ([field fields])
                        (values (hash-ref field 'name) field)))
    (hash-set typedef 'fields fieldsmap)))


(define enumdef->valuesmap
  (lambda (enum)
    (define vals (hash-ref enum 'values))
    (define valuesmap (for/hash ([value vals]
                        #:when (not (member? (substring value 0 1) (list "\"" "'" "`"))))
                        (values value void)))
    (hash-set enum 'values valuesmap)))



(define Schema%
  (class object%
    (super-new)
    (init-field typedefs
                querytype
                [mutationtype 'null]
                [subscriptiontype 'null])

    
　　;;;initialize
    (define _typemap (make-hash))
    (define _querytype 'null)
    (define _mutationtype 'null)
    (define _subscriptiontype 'null)

    (for ([typedef typedefs])
      (let ([kind (hash-ref typedef 'kind)]
            [name (hash-ref typedef 'name)])
        (cond 
          ((eq? kind 'UNION) (hash-set! _typemap name typedef))
          
          ((eq? kind 'SCALAR) (hash-set! _typemap name typedef))
          
          ((eq? kind 'ENUM)
           (hash-set! _typemap name (enumdef->valuesmap typedef)))

          ((eq? kind 'INPUT_OBJECT)
           (hash-set! _typemap name (typedef->fieldsmap typedef)))
          
          ((eq? kind 'OBJECT)
           (hash-set! _typemap name (typedef->fieldsmap typedef)))
          
          (else (error "initialize error: new Schema%")))))

    

    (set! _querytype (typedef->fieldsmap querytype))

    (when mutationtype
      (set! _mutationtype (typedef->fieldsmap mutationtype)))

    (when subscriptiontype
      (set! _subscriptiontype (typedef->fieldsmap subscriptiontype)))



    ;;; メソッド
    (define/public get-roottype
      (lambda (optype)
        (cond
          ((eq? optype 'query) _querytype)
          ((eq? optype 'mutation) _mutationtype)
          ((eq? optype 'subscription) _subscriptiontype)
          (else 'get-roottype))))
    
    
    (define/public get-querytype
      (lambda ()
        _querytype))
    

    (define/public get-mutationtype
      (lambda ()
        _mutationtype))
    

    (define/public get-subscriptiontype
      (lambda ()
        _subscriptiontype))
    

    (define/public get-types
      (lambda ()
        (hash-values _typemap)))
    

    (define/public get-type
      (lambda (typename)
        (hash-ref _typemap typename 'null)))
    

    (define/public get-defined-typenames
      (lambda ()
        (hash-keys _typemap)))
    

    (define/public defined-type?
      (lambda (typename)
        (hash-has-key? _typemap typename)))


    ))
