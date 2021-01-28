#lang racket
(require "utilities.rkt")

(provide queryparse)


(define queryparse
  (lambda (source)
    (let ([parser (new QueryParser% [source source])])
      (send parser parse-document))))


(define QueryParser%
  (class object%
    (super-new)
    (init-field source)
    (define tokens (list->vector (source->tokens source)))
    (define loc 0)
    (define end (vector-length tokens))
    (define error #f)

    ;;調べるトークンを取り出す
    ;;locを進める(advance)ことで次のトークンが取り出せるようになる。
    (define token
      (lambda ()
        (vector-ref tokens loc)))


    (define end-of-token?
      (lambda ()
        (= loc end)))


    (define set-error
      (lambda (errstr . op)
        (define _error (apply make-error (cons errstr op)))
        (set! error _error)
        ""))

    ;;locを+1 
    (define advance
      (lambda ([n 1])
        (unless (end-of-token?)
          (set! loc (+ loc n)))))

    ;;今みているトークンがstrに一致するかどうか?
    ;;一致していたら次のトークン進んでから#tを返す
    (define token-eq?
      (lambda (str)
        (if (string=? (token) str)
            (begin (advance) #t)
            #f)))
    

    (define/public parse-document
      (lambda ([l '()])
        (cond
          (error (hash 'errors error))
          ((end-of-token?) l)
          (else
           (parse-document (cons (parse-definition) l))))))
    

    (define parse-definition
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected Name, found <EOF>."))
          ((member? (token) '("query" "mutation" "subscription" "{") string=?)
           (parse-operation-def))
          ((token-eq? "fragment") (parse-flagment-def))
          (else
           (set-error "Syntax Error: Unexpected Name '~a'." (token))))))
    

    (define parse-flagment-def
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected Name, found <EOF>."))
          (else
            (hash 'kind 'FRAGMENT_DEFINITION
                  'name (parse-name)
                  'typecond (parse-typecond)
                  'selectionset (parse-selectionset))))))

    (define parse-typecond
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected 'on', found <EOF>."))
          ((token-eq? "on") (hash 'name (parse-name)))
          (else
           (set-error "Syntax Error: Expected 'on', found '~a'." (token))))))
             

    (define parse-operation-def
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Unexpected <EOF>."))
          (else
           (hash 'kind 'OPERATION_DEFINITION
                 'operation (parse-op-type)
                 'name (parse-op-name)
                 'selectionset (parse-selectionset))))))
    

    (define parse-op-type
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Unexpected <EOF>."))
          ((string=? (token) "{") 'query)
          (else
           (let ([op (token)])
           (begin (advance) (string->symbol op)))))))
    

    (define parse-op-name
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Unexpected <EOF>."))
          ((string=? (token) "{") 'undefined)
          (else
           (begin (advance) (string->symbol (token)))))))
    
    (define parse-selectionset
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected '{', found <EOF>."))
          ((token-eq? "{") (parse-selectionset*))
          (else null))))
    
    
    (define parse-selectionset* 
      (lambda ([l '()])
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected '}' or Name, found <EOF>."))
          ((token-eq? "}") l)
          (else
           (parse-selectionset* (cons (parse-selection) l))))))
    

    (define parse-selection
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected '}' or Name, found <EOF>."))
          ((token-eq? ".")
           (cond
             ((end-of-token?) (set-error "Syntax Error: Expected '}' or Name, found <EOF>."))
             ((token-eq? ".")
               (cond
                 ((end-of-token?) (set-error "Syntax Error: Expected '}' or Name, found <EOF>."))
                 ((token-eq? ".") (parse-fragment))
                 (else (set-error "Syntax Error: Cannot parse the unexpected character '.'."))))
             (else (set-error "Syntax Error: Cannot parse the unexpected character '.'."))))
          (else (parse-field)))))
    

    (define parse-fragment
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected Name, found <EOF>."))
          ((token-eq? "on") (hash 'kind 'INLINE_FRAGMENT
                                  'typecond (hash 'name (parse-name))
                                  'selectionset (parse-selectionset)))
          (else (hash 'kind 'FRAGMENT_SPREAD
                      'name (parse-name))))))
    

    (define parse-field
      (lambda ()
        (define name (parse-name))
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected '}' or Name, found <EOF>."))
          ((token-eq? ":") (hash 'kind 'FIELD
                                 'alias name
                                 'name (parse-name)
                                 'args (parse-args)
                                 'selectionset (parse-selectionset)))
          (else (hash 'kind 'FIELD
                      'alias 'undefined
                      'name name
                      'args (parse-args)
                      'selectionset (parse-selectionset))))))
    

    (define parse-name
      (lambda ()
        (cond
          (error null)
          (else
           (let ([name (token)])
             (if (legal-name? name)
                 (begin (advance) (string->symbol name))
                 (set-error "Syntax Error: Found illegal Name '~a'." name)))))))
    

    (define parse-args
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected '} or Name', found <EOF>."))
          ((token-eq? "(") (parse-args*))
          (else null))))
    

    (define parse-args*
      (lambda ([l '()])
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected ')' or Name, found <EOF>."))
          ((token-eq? ")") l)
          (else (parse-args* (cons (parse-arg) l))))))
    

    (define parse-arg
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected Name, found <EOF>."))
          (else
           (let ([name (parse-name)])
             (cond
               (error null)
               ((end-of-token?) (set-error "Syntax Error:  Expected ':', found <EOF>."))
               ((token-eq? ":") (hash 'name name 'value (parse-value)))
               (else (set-error "Syntax Error: Expected ':', found '~a'." (token)))))))))
    

    (define parse-value
      (lambda ()
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Unxpected <EOF>."))
          ((token-eq? "[") (parse-list))
          ((token-eq? "{") (parse-object))
          ((token-eq? "true") (hash 'kind 'BOOLEAN 'value 'true))
          ((token-eq? "false") (hash 'kind 'BOOLEAN 'value 'false))
          ((token-eq? "null") (hash 'kind 'NULL 'value 'null))
          ((string-value? (token)) (parse-stringvalue))
          ((number? (string->number (token))) (parse-numbervalue))
          (else
           (let ([value (token)])
             (advance)
             (hash 'kind 'ENUM 'value value))))))
    

    (define parse-list
      (lambda ()
        (if error
            null
            (hash 'kind 'LIST 'value (parse-listvalues)))))
    

    (define parse-listvalues
      (lambda ([l '()])
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected ']', found <EOF>."))
          ((eq? (token) "[") (parse-listvalues (append l (list (parse-list)))))
          ((token-eq? "]") l)
          (else (parse-listvalues (append l (list (parse-value))))))))


    (define parse-object
      (lambda ()
        (if error
            null
            (hash 'kind 'OBJECT 'fields (parse-objectfields)))))
    

    (define parse-objectfields
      (lambda ([l '()])
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected '}' or Name, found <EOF>."))
          ((token-eq? "}") l)
          (else (parse-objectfields (cons (parse-objectfield) l))))))

    
    (define parse-objectfield
      (lambda ()
        (define name (parse-name))
        (cond
          (error null)
          ((end-of-token?) (set-error "Syntax Error: Expected ':', found <EOF>."))
          ((token-eq? ":")
           (hash 'name name 'value (parse-value)))
          (else (set-error "Syntax Error: Expected ':', found '~a'." (token))))))

    
    (define string-value?
      (lambda (token)
        (member? (substring token 0 1) '("\"" "'" "`") string=?)))
    

    (define parse-stringvalue
      (lambda ()
        (cond
          (error null)
          (else
           (let* ([str (token)]
                  [len (string-length str)])
             (advance)
             (hash 'kind 'STRING 'value (substring str 1 (- len 1))))))))  ;;"\"aaa\"" -> "aaa"
    

    (define parse-numbervalue
      (lambda ()
        (cond
          (error null)
          (else
           (let ([num (token)])
             (advance)
             (if (token-eq? ".")
                 (let ([decimal (token)])
                   (advance)
                   (if (number? (string->number decimal))
                       (hash 'kind 'FLOAT 'value (string->number (string-append num "." decimal)))
                       (set-error "Syntax Error: Invalid number, expected digit but got: '~a'."
                                  decimal)))
                 (hash 'kind 'INT 'value (string->number num))))))))
    
    )
  )



(define source->tokens
  (lambda (str)
    (s->ts (string-append str " ") '())))


(define s->ts
  (lambda (str ls)
    (define s (if (string=? str "") #f (substring str 0 1)))
    (cond
      ((not s) ls)
      ((member? s '("{" "}" "(" ")" ":" "[" "]" ".") string=?)
       (s->ts (substring str 1) (append ls (list s))))
      ((member? s '("\n" "\t" "," " ") string=?)
       (s->ts (substring str 1) ls))
      ((member? s '("\"" "'" "`") string=?)
       (let ([end-string (get-index-end-string str)])
         (s->ts (substring str end-string)
                (append ls (list (substring str 0 end-string))))))
      (else
       (let ([end-token (get-index-end-token str)])
         (s->ts (substring str end-token)
                (append ls (list (substring str 0 end-token)))))))))


(define get-index-end-string
  (lambda (str)
    (define x (substring str 0 1)) ;; x: "\"" or "'" or "`" 
    (define (aux str i)
      (let ([s (substring str i (+ i 1))])
        (if (string=? s x)
            (+ i 1)
            (aux str (+ i 1)))))
    (aux str 1)))


(define get-index-end-token
  (lambda (str)
    (define (aux str i)
      (if (member? (substring str i (+ i 1))
                   '("{" "}" "(" ")" ":" "\n" "\t"
                         "[" "]" "," " " "." "\"" "'" "`")
                   string=?)
          i
          (aux str (+ i 1))))
    (aux str 0)))
