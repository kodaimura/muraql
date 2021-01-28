#lang racket

(provide member?
         scalar-type?
         nonnull-type?
         list-type?
         named-type?
         scalar-field?
         scalar?
         enum?
         get-fieldtype-name
         make-error
         legal-name?)


(define member?
  (lambda (x ls [f eq?])
    (cond
      ((null? ls) #f)
      ((f x (car ls)) #t)
      (else (member? x (cdr ls) f)))))


(define scalar-type?
  (lambda (fieldtype)
    (and (eq? (hash-ref fieldtype 'kind) 'NAMED)
         (member? (hash-ref fieldtype 'name)
                  (list 'Int 'Float 'String 'Boolean 'ID)))))


(define nonnull-type?
  (lambda (fieldtype)
    (eq? (hash-ref fieldtype 'kind) 'NON_NULL)))


(define list-type?
  (lambda (fieldtype)
    (eq? (hash-ref fieldtype 'kind) 'LIST)))


(define named-type?
  (lambda (fieldtype)
    (eq? (hash-ref fieldtype 'kind) 'NAMED)))


(define scalar-field?
  (lambda (fielddef)
    (scalar-type? (hash-ref fielddef 'type))))


(define scalar?
  (lambda (typename)
    (member? typename (list 'Int 'Float 'String 'Boolean 'ID))))


(define enum?
  (lambda (typename schema)
    (eq? (hash-ref (send schema get-type typename) 'kind)
         'ENUM)))


(define union?
  (lambda (typename schema)
    (eq? (hash-ref (send schema get-type typename) 'kind)
         'UNION)))


(define get-fieldtype-name
  (lambda (fieldtype)
    (if (eq? 'NAMED (hash-ref fieldtype 'kind))
        (hash-ref fieldtype 'name)
        (get-fieldtype-name (hash-ref fieldtype 'type)))))


(define make-error
  (lambda (str . opt)
    (list (hash 'message (apply format (cons str opt))))))


(define legal-name?
      (lambda (str)
        (define ls (map char->integer (string->list str)))
        (define x (car ls))
        (define rest (cdr ls))
        
        (and (or (= 95 x)
                 (and (< 96 x) (> 123 x))
                 (and (< 64 x) (> 91 x)))
             (null? (filter-not
                     (lambda (n) (or (= 95 x)
                                     (and (< 96 x) (> 123 x))
                                     (and (< 64 x) (> 91 x))
                                     (and (< 47 x) (> 58 x)))) rest)))))


(define ->string
  (lambda (x)
    (cond
      ((string? x) x)
      ((number? x) (number->string x))
      ((symbol? x) (symbol->string x))
      (else '->string))))