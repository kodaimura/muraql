#lang racket

(require racket/math)
(require "utilities.rkt")

(provide coerce-input-value
         coerce-output-scaler
         coerce-output-String)



(define coerce-input-value
  (lambda (value type schema)
    (define kind (hash-ref type 'kind))
    (define name (hash-ref type 'name 'null))
    (cond
      ((eq? kind 'NON_NULL) (coerce-input-Non_Null value type schema))
      ((eq? kind 'LIST) (coerce-input-List value type schema))
      ((eq? kind 'NULL) 'null)
      ((eq? name 'Int) (coerce-input-Int value))
      ((eq? name 'Float) (coerce-input-Float value))
      ((eq? name 'String) (coerce-input-String value))
      ((eq? name 'Boolean) (coerce-input-Boolean value))
      ((eq? name 'ID) (coerce-input-ID value))
      (else (coerce-input-Enum-or-Input value type schema)))))


(define coerce-input-Non_Null
  (lambda (value type schema)
    (if (eq? value 'null)
        (make-error "Expected non-null value but found null")
        (coerce-input-value value (hash-ref type 'type) schema))))


(define coerce-input-List
  (lambda (value type schema)
    (define values (hash-ref value 'value))
    (cond
      ((eq? 'null values) 'null) 
      ((list? values)
       (map (lambda (value) (coerce-input-value value (hash-ref type 'type) schema))
            values))
      (else
       (list (coerce-input-value value (hash-ref type 'type) schema))))))


(define coerce-input-Enum-or-Input
  (lambda (value type schema)
    (define typedef (send schema get-type (hash-ref type 'name)))
    (cond
      ((eq? (hash-ref typedef 'kind) 'INPUT_OBJECT) (coerce-input-Input value typedef schema))
      ((eq? (hash-ref typedef 'kind) 'ENUM) (hash-ref value 'value))
      (else (error 'coerce-input-Enum-or-Input)))))


(define coerce-input-Input
  (lambda (value typedef schema)
    (define fieldsdef (hash-ref typedef 'fields))
    (define fieldmap (make-hash))
    (for ([field (hash-ref value 'fields null)])
      (let ([name (hash-ref field 'name)])
        (hash-set! fieldmap
                   name
                   (coerce-input-value (hash-ref field 'value)
                                       (hash-ref (hash-ref fieldsdef name) 'type)
                                       schema))))
    
    (for ([field (hash-values fieldsdef)])
      (let ([name (hash-ref field 'name)])
        (when (and (not (hash-has-key? fieldmap name))
                   (not (eq? 'null (hash-ref field 'defaultValue))))
          (hash-set! fieldmap
                     name
                     (hash-ref field 'defaultValue)))))
    fieldmap))


(define coerce-output-scaler
  (lambda (value type)
    (define typename (hash-ref type 'name))
    (cond
      ((eq? value 'null) 'null)
      ((eq? typename 'Int) (coerce-output-Int value))
      ((eq? typename 'Float) (coerce-output-Float value))
      ((eq? typename 'String) (coerce-output-String value))
      ((eq? typename 'Boolean) (coerce-output-Boolean value))
      ((eq? typename 'ID) (coerce-output-ID value))
      (else (error 'coerce-output-value)))))


(define integer->Int
  (lambda (val)
    (if (< (abs val) 2147483647)
        (exact-round val)
        (make-error
         "Can not convert non 32bit signed integer value to Int value.:~a" val))))


(define coerce-output-Int
  (lambda (val)
    (cond
      ((integer? val) (integer->Int val))
      ((boolean? val) (if val 1 0))
      ((string? val)
       (let ([num (string->number val)])
         (if (integer? num)
             (integer->Int num)
             (make-error "Can not convert non-Int value to Int value.:~a" val))))
      (else
       (make-error "Can not convert non-Int value to Int value.:~a" val)))))
  

(define coerce-input-Int
  (lambda (value)
    (define val (hash-ref value 'value))
    (if (and (exact-integer? val) (< (abs val) 2147483647))
        val
        (make-error "Can not convert non-Int value to Int value.:~a" val))))


(define coerce-output-Float
  (lambda (val)
    (cond
      ((number? val) (* 1.0 val))
      ((boolean? val) (if val 1.0 0.0))
      ((string? val)
       (let ([num (string->number val)])
         (if (number? num)
             (* 1.0 num)
             (make-error "Can not convert non numeric value to Float value.:~a" val))))
      (else
       (make-error "Can not convert non numeric value to Float value.:~a" val)))))


(define coerce-input-Float
  (lambda (value)
    (define val (hash-ref value 'value))
    (if (number? val)
        (* 1.0 val)
        (make-error "Can not convert non numeric value to Float value.:~a" val))))


(define coerce-output-String
  (lambda (val)
    (cond
      ((string? val) val)
      ((number? val) (number->string val))
      ((boolean? val) (if val "true" "false"))
      ((symbol? val) (symbol->string val))
      (else
       (make-error "Can not convert non String value to String value.:~a" val)))))

(define coerce-input-String
  (lambda (value)
    (define val (hash-ref value 'value))
    (if (string? val)
        val
        (make-error "Can not convert non String value to String value.:~a" val))))


(define coerce-output-Boolean
  (lambda (val)
    (cond
      ((boolean? val) val)
      ((number? val) (not (zero? val)))
      (else
       (make-error "Can not convert non Boolean value to Boolean value.:~a" val)))))


(define coerce-input-Boolean
  (lambda (value)
    (define val (hash-ref value 'value))
    (cond
      ((eq? 'true val) #t)
      ((eq? 'false val) #f)
      (else
       (make-error "Can not convert non Boolean value to Boolean value.:~a" val)))))


(define coerce-output-ID
  (lambda (val)
    (cond
      ((string? val) val)
      ((exact-integer? val) (number->string val))
      (else
       (make-error "Can not convert non ID value to ID value.:~a" val)))))

(define coerce-input-ID
  (lambda (value)
    (define val (hash-ref value 'value))
    (cond
      ((string? val) val)
      ((exact-integer? val) (number->string val))
      (else
       (make-error "Can not convert non ID value to ID value.:~a" val)))))
