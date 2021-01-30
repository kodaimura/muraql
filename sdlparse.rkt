#lang racket
;; schemaをパースする

(require "utilities.rkt")

(provide sdlparse)


;(sdlparse (file->string "schema.graphql"))
;(sdlparse "type Query { users: [User]}")
(define sdlparse
  (lambda (source)
    (define parser (new SDLParser% [source source]))
    (send parser parse-document)))


(define SDLParser%
  (class object%
    (super-new)
    (init-field source)

    (define tokens (list->vector (source->tokens source)))
    (define loc 0)
    (define end (vector-length tokens))

    ;;調べるトークンを取り出す
    ;;locを進める(advance)ことで次のトークンが取り出せるようになる。
    (define token
      (lambda ()
        (if (<= end loc)
            (error "Syntax Error: Unexpected <EOF>")
            (vector-ref tokens loc))))

    ;;locを+1 
    (define advance
      (lambda ([n 1])
        (set! loc (+ loc n))))

    ;;今みているトークンがstrに一致するかどうか?
    ;;一致していたら次のトークン進んでから#tを返す
    (define token-eq?
      (lambda (str)
        (if (string=? (token) str)
            (begin (advance) #t)
            #f)))
    

    (define parse-name
      (lambda ()
        (define name (token))
        (if (legal-name? name)
            (begin (advance) (string->symbol name))
            (error "Syntax Error: Expected Name, found illegal Name " name))))

    
    (define/public parse-document
      (lambda ([l '()])
        (if (= end loc)
            l
            (parse-document (cons (parse-definition) l)))))
    

    (define parse-definition
      (lambda ()
        (cond
          ((member? (token) '("type" "union" "enum" "input") string=?) (parse-type-def))
          ((token-eq? "schema") (parse-schema-def))
          (else (error "Syntax Error: Unexpected " (token))))))


    (define parse-type-def
      (lambda ()
        (cond
          ((token-eq? "type") (parse-objecttype-def))
          ((token-eq? "union") (parse-uniontype-def))
          ((token-eq? "enum") (parse-enumtype-def))
          ((token-eq? "input") (parse-inputtype-def))
          (else (error "Syntax Error: Unexpected " (token))))))
    

    (define parse-objecttype-def
      (lambda ()
        (define name (parse-name))
        (if (token-eq? "{")
            (hash 'kind 'OBJECT 'name name 'description 'null
                  'fields (parse-fields-def))
            (error "Syntax Error: Expected '{', found " (token)))))

    
    (define parse-fields-def
      (lambda ([l '()])
        (if (token-eq? "}")
            l
            (parse-fields-def (cons (parse-field-def) l)))))
    

    (define parse-field-def
      (lambda ()
        (define name (parse-name))
        (define args (if (token-eq? "(") (parse-args-def) '()))
        (if (token-eq? ":")
            (hash 'name name 'description 'null
                  'args args 'type (parse-type-reference))
            (error "Syntax Error: Expected ':', found " (token)))))
    

    (define parse-args-def
      (lambda ([l '()])
        (if (token-eq? ")")
            l
            (parse-args-def (cons (parse-inputvalue-def) l)))))
    

    (define parse-inputvalue-def
      (lambda ()
        (define name (parse-name))
        (cond
          ((token-eq? ":")
           (let ([type (parse-type-reference)])
             (if (token-eq? "=")
                 (hash 'name name 'type type 'defaultValue (parse-value))
                 (hash 'name name 'type type 'defaultValue 'null))))
          (else (error "Syntax Error: Expected ':', found " (token))))))
    

    (define parse-value
      (lambda ()
        (cond
          ((string->number (token)) (let ([val (string->number (token))])
                                      (advance) val))
          ((token-eq? "true") 'true)
          ((token-eq? "false") 'false)
          ((token-eq? "null") 'null)
          ((string-value? (token)) (parse-stringvalue))
          (else
           (let ([value (string->symbol (token))])
             (advance) value)))))
    

    (define string-value?
      (lambda (token)
        (member? (substring token 0 1) '("\"" "'" "`") string=?)))
    

    (define parse-stringvalue
      (lambda ()
        (define str (token))
        (define len (string-length str))
        (advance)
        (substring str 1 (- len 1))))  ;;"\"aaa\"" -> "aaa"


    (define type "")
    (define parse-type-reference
      (lambda ()
        (cond
          ((token-eq? "[")
           (begin (set! type (parse-type-reference))
                  (if (token-eq? "]")
                      (set! type (hash 'kind 'LIST 'type type))
                      (error "Syntax Error: Expected ']', found " (token)))))
          (else
           (set! type (hash 'kind 'NAMED 'name (parse-name)))))
        
        (if (token-eq? "!")
            (hash 'kind 'NON_NULL 'type type)
            type)))
    

    (define parse-schema-def
      (lambda ()
        (if (token-eq? "{")
            (hash 'kind 'schema 'fields (parse-operationtypes-def))
            (error "Syntax Error: Expected '{', found " (token)))))
    

    (define parse-operationtypes-def
      (lambda ([l '()])
        (if (token-eq? "}")
            l
            (parse-operationtypes-def (cons (parse-operationtype-def) l)))))
    

    (define parse-operationtype-def
      (lambda ()
        (cond
          ((member? (token) '("query" "mutation" "subscription") string=?)
           (let ([operation (string->symbol (token))])
             (advance)
             (if (token-eq? ":")
                 (hash 'operation operation 'typename (parse-name))
                 (error "Syntax Error: Expected ':', found " (token)))))
          (else
           (error "Syntax Error: Unexpected " (token))))))

    
    (define parse-uniontype-def
      (lambda ()
        (define name (parse-name))
        (if (token-eq? "=")
            (hash 'kind 'UNION 'name name 'types (parse-uniontypes))
            (error "Syntax Error: Expected '=', found " (token)))))
    

    (define parse-uniontypes
      (lambda ()
        (define name (parse-name))
        (if (or (= end loc) (not (token-eq? "|")))
            (list name)
            (cons name (parse-uniontypes)))))
    

    (define parse-enumtype-def
      (lambda ()
        (define name (parse-name))
        (define args (if (token-eq? "(") (parse-args-def) '()))
        (if (token-eq? "{")
            (hash 'kind 'ENUM 'name name 'args args
                  'values (parse-enumvalues))
            (error "Syntax Error: Expected '{', found " (token)))))
    

    (define parse-enumvalues
      (lambda ([l '()])
        (if (token-eq? "}")
            l
            (parse-enumvalues (cons (parse-enumvalue) l)))))
    

    (define parse-enumvalue
      (lambda ()
        (define value (token))
        (advance)
        value))
    

    (define parse-inputtype-def
      (lambda ()
        (define name (parse-name))
        (if (token-eq? "{")
            (hash 'kind 'INPUT_OBJECT 'name name 'description 'null
                  'fields (parse-inputfields-def))
            (error "Syntax Error: Expected '{', found " (token)))))
    

    (define parse-inputfields-def
      (lambda ([l '()])
        (if (token-eq? "}")
            l
            (parse-inputfields-def (cons (parse-inputfield-def) l)))))

    
    (define parse-inputfield-def
      (lambda ()
        (define name (parse-name))
        (cond
          ((token-eq? ":")
           (let ([type (parse-type-reference)])
             (if (token-eq? "=")
                 (hash 'name name 'description 'null
                       'type type 'defaultValue (parse-value))
                 (hash 'name name 'description 'null
                       'type type 'defaultValue 'null))))
          (else
           (error "Syntax Error: Expected ':', found " (token))))))
    )
  )
