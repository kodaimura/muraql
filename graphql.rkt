#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/http/request-structs
         web-server/http/empty
         json)

(require file/sha1)

(require "utilities.rkt"
         "execute.rkt"
         "sdlparse.rkt"
         "queryparse.rkt"
         "schema.rkt"
         "dbconnect.rkt"
         "validate.rkt")


(provide query-run
         query-get
         query-all)

(provide define/schema
         define/resolver
         d/r)

(provide graphql
         run-graphql
         set-spin-graphql)


(define __typedefs '())
(define __resolvers (make-hash))
(define __schema #f)
(define cache (make-hash))


(define schema-set!
  (lambda ([typedefs __typedefs] [resolvers __resolvers])
    (set! __schema (build-schema typedefs resolvers))))


;; (define/sdl (file->string "schema.graphql"))
(define define/schema
  (lambda (str)
    (set! __typedefs str)))


;; (define/resolver Type.field
;;  (lambda (parent args info) ...))
(define-syntax-rule (define/resolver type.field proc)
    (let* ([t.f (quote type.field)]
           [tf (string-split (symbol->string t.f) ".")]
           [type (string->symbol (car tf))]
           [field (string->symbol (second tf))]
           [tr (hash-ref __resolvers type #f)])
      (cond 
        (tr (if (hash-has-key? tr field)
                (error 'resolver "Duplicate: ~a" t.f)
                (hash-set! tr field proc)))
        (else (hash-set! __resolvers type (make-hash (list (cons field proc))))))))

;; (d/r Type.field
;;  (lambda (parent args info) ...))
;; (define/resolver) と同じ
(define-syntax-rule (d/r type.field proc)
    (let* ([t.f (quote type.field)]
           [tf (string-split (symbol->string t.f) ".")]
           [type (string->symbol (car tf))]
           [field (string->symbol (second tf))]
           [tr (hash-ref __resolvers type #f)])
      (cond 
        (tr (if (hash-has-key? tr field)
                (error 'resolver "Duplicate: ~a" t.f)
                (hash-set! tr field proc)))
        (else (hash-set! __resolvers type (make-hash (list (cons field proc))))))))

(define make-cachekey
  (lambda (query opname)
    (define tokens (if opname
                       (cons opname (source->tokens query))
                       (source->tokens query)))
    (bytes->hex-string
     (sha224-bytes
      (string->bytes/utf-8 (string-join tokens))))))

;; graphql メイン
(define graphql
  (lambda (query opname)
    (define cachekey (make-cachekey query opname))
    (define document (queryparse query))
    (cond
      ((hash? document) document)  ;; #hash((errors . '( ... ))) 
      (else
       (let ([err (validate document __schema)])
         (if (null? err)
             (execute __schema __resolvers document opname cachekey)
             (hash 'error (hash 'errors err))))))))


;; graphql処理の起点
(define execute-request
  (lambda (req)
    (define post-data (if (bytes=? (request-method req) #"POST")
                          (bytes->jsexpr (request-post-data/raw req))
                          (make-hash (url-query (request-uri req)))))
    (define query (hash-ref post-data 'query))
    (define opname (hash-ref post-data 'operationName #f))
    (graphql query opname)))


(define response/json
  (lambda (body)
    (response/jsexpr
     #:headers
     (list (make-header #"Access-Control-Allow-Origin" #"*")
           (make-header #"Connection" #"keep-alive"))
     body)))


;;オリジン間リソース共有(cors)
(define response/preflight
  (lambda ()
    (response/empty
     #:headers
     (list (make-header #"Access-Control-Allow-Origin" #"*")
           (make-header #"Access-Control-Allow-Methods" #"GET,HEAD,PUT,PATCH,POST,DELETE")
           (make-header #"Access-Control-Allow-Headers" #"content-type")
           (make-header #"Connection" #"keep-alive")))))


(define preflight-request?
  (lambda (req)
    (bytes=? (request-method req) #"OPTIONS")))
       
(define graphql*
  (lambda (req)
    (if (preflight-request? req)
        (response/preflight)
        (response/json (execute-request req)))))


;; "/graphql" エンドポイント起動
(define run-graphql
  (lambda ([port 4000])
    (schema-set!)
    (serve/servlet #:servlet-path "/graphql"
                   #:port port
                   #:launch-browser? #f
                   #:mime-types-path "application/json"
                   graphql*)))


;; dmac/spin に"/graphql"エンドポイント追加
(define set-spin-graphql
  (lambda (define-handler [endpoint "/graphql"])
    
    (schema-set!)
    
    (define (json-response-maker status headers body)
      (response/json body))
    
    (define (preflight-response-maker status headers body)
      (response/preflight))
    
    (define (post* path handler)
      (define-handler "POST" path handler json-response-maker))
    
    (define (options* path handler)
      (define-handler "OPTIONS" path handler preflight-response-maker))

    (define (get* path handler)
      (define-handler "GET" path handler json-response-maker))
    
    (post* endpoint
           (lambda (req)
             (execute-request req)))

    (get* endpoint
           (lambda (req)
             (execute-request req))) 
    
    (options* endpoint
              (lambda (req) ""))))

