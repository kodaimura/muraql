#lang racket
(require "utilities.rkt"
         "validate_items.rkt")

(provide validate)


(define op-definition?
  (lambda (definition)
    (eq? 'OPERATION_DEFINITION (hash-ref definition 'kind))))


(define fr-definition?
  (lambda (definition)
    (eq? 'FRAGMENT_DEFINITION (hash-ref definition 'kind))))


(define validate
  (lambda (document schema)
    (define opcount (length (filter op-definition? document)))
    (define frs (filter fr-definition? document))
    (define error (fragment-spreads-not-form-cycles? frs))
    (define fragments (for/hash ([fragment frs])
                        (values (hash-ref fragment 'name) fragment)))
    
    (if (null? error)
        (validate-definitions document opcount fragments schema)
        error)))


(define validate-definitions
  (lambda (definitions opcount fragments schema)
    (define opnames (make-hash))
    (define frnames (make-hash))

    (define errors '())

    (for ([definition definitions])
      (let ([error (validate-definition
                    definition opnames frnames opcount fragments schema)])
        (set! errors (append errors error))))

    errors))
    

(define validate-definition
  (lambda (definition opnames frnames opcount fragments schema)
    (define name (hash-ref definition 'name))
    
    (if (op-definition? definition)
        (let ([errors (validate-operation
                       definition opnames opcount fragments schema)])
          (hash-set! opnames name void)
          errors)
        (let ([errors (validate-fragment
                       definition frnames fragments schema)])
          (hash-set! frnames name void)
          errors))))


(define validate-operation
  (lambda (operation opnames opcount fragments schema)
    (define roottype (send schema get-roottype (hash-ref operation 'operation)))
    (define selectionset (hash-ref operation 'selectionset))
    
    (append (unique-operation-name? operation opnames)
            (lone-anonymous-operation? operation opcount)
            (validate-fields selectionset roottype fragments schema))))


(define validate-fragment
  (lambda (fragment frnames fragments schema)
    (define error1 (unique-fragment-name? fragment frnames))
    (define error2 (fragment-target-type-defined-on-schema? fragment schema))
    (define error3 (fragment-target-objecttype? fragment schema))

    (if (not (null? (append error2 error3)))
        (append error2 error3 error1)
        (let ([selectionset (hash-ref fragment 'selectionset)]
              [targettype (send schema get-type
                                (hash-ref (hash-ref fragment 'typecond) 'name))])
          (validate-fragment-fields selectionset targettype fragments schema)))))


(define validate-fragment-fields
  (lambda (selectionset targettype fragments schema)
    (validate-fields selectionset targettype fragments schema)))


(define fragment-spread?
  (lambda (selection)
    (eq? (hash-ref selection 'kind) 'FRAGMENT_SPREAD)))


(define inline-fragment?
  (lambda (selection)
    (eq? (hash-ref selection 'kind) 'INLINE_FRAGMENT)))


(define validate-fields
  (lambda (selectionset parenttype fragments schema
                        [names (make-hash)] [aliases (make-hash)])
    (define errors '())
    
    (for ([selection selectionset])
      (let ([error
             (cond
               ((fragment-spread? selection)
                (validate-fragmentspread
                 selection parenttype fragments schema names aliases))
               
               ((inline-fragment? selection)
                (validate-inlinefragment
                 selection parenttype fragments schema names aliases))
               
               (else
                (validate-field
                 selection parenttype fragments schema names aliases)))])

        (set! errors (append errors error))))

    errors))


(define validate-fragmentspread
  (lambda (selection parenttype fragments schema names aliases)
    (define error1 (fragment-defined-on-document? selection fragments))
    (cond
      ((not (null? error1)) error1)
      (else
       (let ([error2 (fragment-spread-match-typecond?
                      selection parenttype fragments)]
             [error3 (validate-fragmentspread-fieldsname
                      selection parenttype fragments schema names aliases)])
          (append error2 error3))))))


(define validate-fragmentspread-fieldsname
  (lambda (selection parenttype fragments schema names aliases)
    (define errors '())
    (define fragment (hash-ref fragments (hash-ref selection 'name)))
    
    (for ([selection (hash-ref fragment 'selectionset)])
      (let ([error (validate-fragment-fieldname
                    selection parenttype fragments schema names aliases)])
        (set! errors (append errors error))))

    errors))


(define validate-inlinefragment-fieldsname
  (lambda (_selection parenttype fragments schema names aliases)
    (define errors '())

    (for ([selection (hash-ref _selection 'selectionset)])
      (let ([error (validate-fragment-fieldname
                    selection parenttype fragments schema names aliases)])
        (set! errors (append errors error))))
    errors))

  
(define validate-fragment-fieldname
  (lambda (selection parenttype fragments schema names aliases)
    (cond
      ((fragment-spread? selection)
       (validate-fragmentspread selection parenttype fragments schema names aliases))
      
      ((inline-fragment? selection)
       (validate-inlinefragment selection parenttype fragments schema names aliases))
      
      (else (validate-fieldname selection names aliases)))))


(define validate-inlinefragment
  (lambda (selection parenttype fragments schema names aliases)
    (define error1 (inline-fragment-target-type-defined-on-schema?
                    selection schema))
    (define error2 (inline-fragment-target-objecttype?
                    selection schema))
    (define error3 (inline-fragment-match-typecond?
                    selection parenttype schema))

    (cond
      ((not (null? (append error1 error2 error3)))
       (let ([error4 (validate-inlinefragment-fieldsname
                      selection parenttype fragments schema names aliases)])
         (append error4 error1 error2 error3)))
      (else
       (let* ([typecond (hash-ref (hash-ref selection 'typecond) 'name)]
              [parenttype* (send schema get-type typecond)])
         (validate-fields
          (hash-ref selection 'selectionset) parenttype* fragments schema names aliases))))))


(define validate-fieldname
  (lambda (selection names aliases)
    (define name (hash-ref selection 'name))
    (define alias (hash-ref selection 'alias))
    
    (define error (response-name-conflict? selection names aliases))
    
    (if (eq? alias 'undefined)
        (hash-set! names name void)
        (hash-set! aliases alias name))

    error))


(define validate-field
  (lambda (selection parenttype fragments schema names aliases)
    (define error1 (validate-fieldname selection names aliases))
    (define error2 (field-defined-on-type? selection parenttype))
    (cond
      ((eq? (hash-ref selection 'name) '__typename) null)
      ((and (member? (hash-ref selection 'name)
                     (list '__schema '__type))
            (eq? (hash-ref (send schema get-querytype) 'name)
                 (hash-ref parenttype 'name))) null)
      ((not (null? error2)) (append error1 error2))
      (else
       (let ([error3 (validate-field* selection parenttype fragments schema)])
         (append error1 error3))))))
    

(define validate-field*
  (lambda (selection parenttype fragments schema)
    (define error1 (validate-args selection parenttype schema))
    (define error2 (if (null? (hash-ref selection 'selectionset))
                       (validate-leaf-field selection parenttype schema)
                       (validate-nonleaf-field selection parenttype fragments schema)))
    
    (append error1 error2))) 


(define validate-leaf-field
  (lambda (selection parenttype schema)
    (leaf-field-scalar? selection parenttype schema)))


(define validate-nonleaf-field
  (lambda (selection parenttype fragments schema)
    (define error (nonleaf-field-object? selection parenttype schema))

    (if (not (null? error))
        error
        (let* ([field (hash-ref (hash-ref parenttype 'fields)
                                (hash-ref selection 'name))]
               [selectionset (hash-ref selection 'selectionset)]
               [type (send schema get-type
                           (get-fieldtype-name (hash-ref field 'type)))])
          
          (validate-fields selectionset type fragments schema)))))
                        
 
(define validate-args
  (lambda (selection parenttype schema)
    (define field (hash-ref (hash-ref parenttype 'fields)
                            (hash-ref selection 'name)))
    (define args (hash-ref selection 'args))
    (define argnames (for/hash ([arg args])
                       (values (hash-ref arg 'name) void)))
    
    (append (validate-args* args field parenttype schema)
            (all-arguments-provided? argnames field))))


(define validate-args*
  (lambda (args field parenttype schema)
    (define names (make-hash))
    (define errors '())

    (for ([arg args])
      (let ([error (validate-arg arg field parenttype names schema)])
        (set! errors (append errors error))
        (hash-set! names (hash-ref arg 'name) void)))

    errors))


(define validate-arg
  (lambda (arg field parenttype names schema)
    (define error1 (argument-defined-on-field? arg field parenttype))
    (define error2 (unique-argument-name? arg names))
    (define errors (append error1 error2))
    (if (null? errors)
        (validate-value arg field schema)
        errors)))


(define validate-value
  (lambda (arg fielddef schema)
    (define value (hash-ref arg 'value))
    (define argdef (findf (lambda (x) (eq? (hash-ref x 'name) (hash-ref arg 'name)))
                          (hash-ref fielddef 'args)))
    (inputvalue-coercible? value (hash-ref argdef 'type) schema)))

