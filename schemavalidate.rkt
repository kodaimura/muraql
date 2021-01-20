#lang racket

(require "utilities.rkt")

(provide validate-schema
         validate-resolvers)


(define is-input-type?
  (lambda (type alltypes)
    (cond
      ((member? (hash-ref type 'kind) (list 'NON_NULL 'LIST))
       (is-input-type? (hash-ref type 'type) alltypes))
      (else
        (let ([name (hash-ref type 'name)])
         (or (scalar? name)
             (member? (hash-ref (hash-ref alltypes name) 'kind)
                      (list 'ENUM 'INPUT_OBJECT))))))))


(define is-output-type?
  (lambda (type alltypes)
    (cond
      ((member? (hash-ref type 'kind) (list 'NON_NULL 'LIST))
       (is-output-type? (hash-ref type 'type) alltypes))
      (else
       (let ([name (hash-ref type 'name)])
         (or (scalar? name)
             (member? (hash-ref (hash-ref alltypes name) 'kind)
                      (list 'OBJECT 'UNION 'ENUM 'INTERFACE))))))))


(define validate-schema
  (lambda (typesAst)
    (define schemaobj (filter (lambda (x) (eq? (hash-ref x 'kind) 'schema))
                              typesAst))
    (define typedefs (filter-not (lambda (x) (eq? (hash-ref x 'kind) 'schema))
                                 typesAst))
    (define typesmap (for/hash ([type typedefs])
                        (values (hash-ref type 'name) type)))

    (define count-schemaobj (length schemaobj))
    
    (when (= 1 count-schemaobj)
      (validate-schemaobj (car schemaobj) typesmap))

    (when (< 1 count-schemaobj)
      (error 'schema-error
              "Must provide only one schema definition."))

    (when (and (= 0 count-schemaobj)
               (not (hash-has-key? typesmap 'Query)))
      (error 'schema-error
              "Query root type must be provided."))

    (validate-typedefs typedefs typesmap)))


(define validate-schemaobj
  (lambda (schema alltypes)
    (define verified (make-hash))
    (for ([field (hash-ref schema 'fields)])
      (validate-schemafield field alltypes verified)
      (hash-set! verified (hash-ref field 'operation) void))
    (unless (hash-has-key? verified 'query)
      (error 'schema-error
             "Query root type must be provided."))))


(define validate-schemafield
  (lambda (field alltypes verified)
    (define operation (hash-ref field 'operation))
    (define typename (hash-ref field 'typename))
    (when (hash-has-key? verified operation)
      (error 'schema-error
             (format "There can be only one ~a type in schema." operation)))
    (when (scalar? typename)
      (error 'schema-error
             (format "Root type must be Object type, it cannot be ~a." typename)))
    (unless (hash-has-key? alltypes typename)
      (error 'schema-error
             (format "Unknown type '~a'." typename)))))


(define validate-typedefs
  (lambda (typedefs alltypes)
    (define verified (make-hash))
    (for ([typedef typedefs])
      (validate-typedef typedef alltypes verified)
      (hash-set! verified (hash-ref typedef 'name) void))))


(define validate-name
  (lambda (name)
    (define name* (symbol->string name))
    (cond
      ((= (string-length name*) 1))
      ((string=? "__" (substring name* 0 2))
       (error 'schema-error
              "Name '~a' must not begin with '__'." name*))
      (else void))))


(define validate-typedef
  (lambda (typedef alltypes verified)
    (define name (hash-ref typedef 'name))
    (define kind (hash-ref typedef 'kind))
    (when (hash-has-key? verified name)
      (error 'schema-error
             (format "There can be only one type named '~a'." name)))
    (validate-name name)
    (cond
      ((eq? kind 'OBJECT) (validate-objecttype typedef alltypes))
      ((eq? kind 'ENUM) (validate-enumtype typedef alltypes))
      ((eq? kind 'UNION) (validate-uniontype typedef alltypes))
      ((eq? kind 'INPUT) (validate-inputtype typedef alltypes))
      (else 'validate-typedef))))


(define validate-objecttype
  (lambda (typedef alltypes)
    (define name (hash-ref typedef 'name))
    (define fields (hash-ref typedef 'fields))
    (validate-name name)
    (validate-object-fields fields typedef alltypes)))


(define validate-object-fields
  (lambda (fields typedef alltypes)
    (define verified (make-hash))
    (when (null? fields)
      (error 'schema-error
             (format "Object type must define one or more fields. :~a"
                     (hash-ref typedef 'name))))
    (for ([field fields])
      (validate-object-field field typedef alltypes verified)
      (hash-set! verified (hash-ref field 'name) void))))


(define validate-object-field
  (lambda (field typedef alltypes verified)
    (define name (hash-ref field 'name))
      
    (when (hash-has-key? verified name)
      (error 'schema-error
             (format "Field '~a.~a' can only be defined once."
                     (hash-ref typedef 'name) name)))

    (validate-name name)
    (validate-object-fieldtype field typedef alltypes)
    (validate-args (hash-ref field 'args) field alltypes)))


(define validate-object-fieldtype
  (lambda (field typedef alltypes)
    (define fieldtype (hash-ref field 'type))
    (define fieldtypename (get-fieldtype-name fieldtype))

    (unless (or (hash-has-key? alltypes fieldtypename) (scalar? fieldtypename))
      (error 'schema-error
             (format "Unknown type '~a'." fieldtypename)))

    (unless (is-output-type? fieldtype alltypes)
      (error 'schema-error
             (format "Field-Type must be either Object, Scalar, Union, Enum, or Interface.: ~a.~a"
                     (hash-ref typedef 'name) (hash-ref field 'name))))))
 
    
(define validate-args
  (lambda (args field alltypes)
    (define verified (make-hash))
    (for ([arg args])
      (validate-arg arg alltypes verified)
      (hash-set! verified (hash-ref arg 'name) void))))


(define validate-arg
  (lambda (arg alltypes verified)
    (define name (hash-ref arg 'name))
    (when (hash-has-key? verified name)
      (error 'schema-error
             "Arg '~a' can only be defined once in same field." name))
    (validate-name name)
    (validate-argtype arg alltypes)))


(define validate-argtype
  (lambda (arg alltypes [x "Argument-Type"])
    (define argtype (hash-ref arg 'type))
    (define argtypename (get-fieldtype-name argtype))

    (unless (or (hash-has-key? alltypes argtypename) (scalar? argtypename))
      (error 'schema-error
             (format "Unknown type '~a'." argtypename)))

    (unless (is-input-type? argtype alltypes)
      (error 'schema-error
             (format "~a must be either Scalar, Enum or Input.:~a"
                     x (hash-ref arg 'name))))))


(define validate-uniontype
  (lambda (typedef alltypes)
    (validate-name (hash-ref typedef 'name))
    (define types (hash-ref typedef 'types))
    
    (when (null? types)
      (error 'schema-error
             (format "Union type must define one or more types. :~a"
                     (hash-ref typedef 'name))))
    
    (for ([type types])
      (validate-uniontype-each-type type typedef alltypes))))


(define validate-uniontype-each-type
  (lambda (type typedef alltypes)
    (when (or (scalar? type)
              (member? (hash-ref (hash-ref alltypes type) 'kind)
                       (list 'UNION 'INTERFACE)))
      (error 'schema-error
             (format "Union type ~a can only include Object types, it cannot include ~a."
                     (hash-ref typedef 'name) type)))
      
    (unless (hash-has-key? alltypes type)
      (error 'schema-error
             "Unknown type '~a'." type))))


(define validate-enumtype
  (lambda (typedef alltypes)
    (validate-name (hash-ref typedef 'name))
    (when (null? (hash-ref typedef 'values))
      (error 'schema-error
             (format "Enum type must define one or more values. :~a"
                     (hash-ref typedef 'name))))))


(define validate-inputtype
  (lambda (typedef alltypes)
    (define verified (make-hash))
    (validate-name (hash-ref typedef 'name))
    (define fields (hash-ref typedef 'fields))
    
    (when (null? fields)
      (error 'schema-error
             (format "Input type must define one or more fields. :~a"
                     (hash-ref typedef 'name))))
    
    (for ([field fields])
      (validate-inputtype-field field typedef alltypes verified)
      (hash-set! verified (hash-ref field 'name) void))))


(define validate-inputtype-field
  (lambda (field typedef alltypes verified)
    (define name (hash-ref field 'name))

    (when (hash-has-key? verified name)
      (error 'schema-error
             (format "Field '~a.~a' can only be defined once."
                      (hash-ref typedef 'name) name)))

    (validate-name name)
    (validate-argtype field alltypes "Input-Type")))


(define validate-resolvers
  (lambda (typesAst resolvers)
    (define typedefs (filter-not (lambda (x) (eq? (hash-ref x 'kind) 'schema))
                                 typesAst))
    (define unions (filter (lambda (def) (eq? 'UNION (hash-ref def 'kind)))
                           typedefs))
    (validate-resolvers-types typedefs resolvers)
    (validate-resolvers-unions unions resolvers)))


(define validate-resolvers-types
  (lambda (typedefs resolvers)
    (define typesmap (for/hash ([type typedefs])
                       (values (hash-ref type 'name)
                               type)))
    (for ([type (hash-keys resolvers)])
      (if (not (hash-has-key? typesmap type))
          (error 'resolver-error
                 "Unknown type '~a'." type)
          (when (eq? (hash-ref (hash-ref typesmap type) 'kind) 'OBJECT)
            (validate-resolvers-fields (hash-ref (hash-ref typesmap type) 'fields)
                                       (hash-ref resolvers type)
                                       type))))))


(define validate-resolvers-fields
  (lambda (fields resolvers typename)
    (define fieldsmap (for/hash ([field fields])
                        (values (hash-ref field 'name)
                                field)))
    (for ([fname (hash-keys resolvers)])
      (unless (hash-has-key? fieldsmap fname)
        (error 'resolver-error
               "~a.~a defined in resolvers, but not in schema" typename fname)))))


(define validate-resolvers-unions
  (lambda (unions resolvers)
    (for ([union unions])
      (let ([name (hash-ref union 'name)])
        (when (or (not (hash-has-key? resolvers name))
                  (not (hash-has-key? (hash-ref resolvers name) '__resolveType)))
            (error 'resolver-error
                   "Type '~a' is missing a '__resolveType' resolver." name))))))



