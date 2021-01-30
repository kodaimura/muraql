#lang racket

(require db)

(provide query-run
         query-get
         query-all)


(define row->hash
  (lambda (row keys)
    (for/hash ([key keys][val row])
      (if (sql-null? val)
          (values key 'null)
          (values key val)))))

(define rows-result->hash
  (lambda (data)
    (define headers (rows-result-headers data))
    (define rows (rows-result-rows data))
    (define keys (map (lambda (header) (string->symbol (cdar header))) headers))
    (map (lambda (row) (row->hash row keys)) rows)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (query-all sqlite3 "select * from author;")
; ->'(#hash((age . 34) (id . 1) (name . "aaa"))
;     #hash((age . 39) (id . 2) (name . "bbb")) 
;     #hash((age . null) (id . 3) (name . "ccc")))

; (query-all sqlite3 "select * from author where id = 1;")
; ->'(#hash((age . 34) (id . 1) (name . "aaa")))

; (query-get sqlite3 "select * from author where id = 1;")
; ->'#hash((age . 34) (id . 1) (name . "aaa"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SQLクエリ実行
;; select以外
(define query-run
  (lambda (connection stmt . args)
    (define sql (apply format (cons stmt args)))
    (query-exec connection sql)))


;; select
;; 条件に合うデータを一つだけ取得
(define query-get
  (lambda (connection stmt . args)
    (define sql (apply format (cons stmt args)))
    (define result (query connection sql))
    (define rows (rows-result->hash result))
    (if (null? rows) '() (car rows))))

;; select
;; 条件に合うデータを全て取得
(define query-all
  (lambda (connection stmt . args)
    (define sql (apply format (cons stmt args)))
    (define result (query connection sql))
    (rows-result->hash result)))
