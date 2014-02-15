#lang racket/base
(require racket/contract net/url xml racket/set)
(module+ test (require rackunit))
(require "len.rkt" "try.rkt")

(provide (contract-out
          [->int (any/c . -> . integer?)]
          [->string (any/c . -> . string?)]
          [->symbol (any/c . -> . symbol?)]
          [->path (any/c . -> . path?)]
          [->complete-path (any/c . -> . complete-path?)]
          [->url (any/c . -> . url?)]
          [->list (any/c . -> . list?)]
          [->vector (any/c . -> . vector?)]
          [->boolean (any/c . -> . boolean?)]))

;; general way of coercing to integer
(define (->int x)
  (cond
    [(integer? x) x]
    [(real? x) (floor x)]
    [(and (string? x) (> (len x) 0)) (->int (string->number x))]
    [(symbol? x) (->int (->string x))]
    [(char? x) (char->integer x)]
    [else (try (len x) (except [exn:fail? (λ(e) (error "Can't convert to integer:" x))]))]))


;; general way of coercing to string
(define (->string x)
  (cond 
    [(string? x) x]
    [(equal? '() x) ""]
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]
    [(url? x) (->string (->path x))] ; todo: a url is more than just a path-string ... it has character encoding issues
    [(path? x) (path->string x)]
    [(char? x) (format "~a" x)]
    [(xexpr? x) (xexpr->string x)] ; put this last so other xexprish things don't get caught
    [else (error (format "Can't make ~a into string" x))]))


;; general way of coercing to symbol
(define (->symbol thing)
  ; todo: on bad input, it will pop a string error rather than symbol error
  (string->symbol (->string thing))) 

;; general way of coercing to path
(define (->path thing)
  ; todo: on bad input, it will pop a string error rather than path error
  (cond 
    [(url? thing) (apply build-path (map path/param-path (url-path thing)))]
    [else (string->path (->string thing))]))


;; general way of coercing to url
(define (->url thing)
  ; todo: on bad input, it will pop a string error rather than url error
  (string->url (->string thing)))

(define (->complete-path thing)
  (path->complete-path (->path thing)))


;; general way of coercing to a list
(define (->list x)
  (cond 
    [(list? x) x]
    [(vector? x) (vector->list x)]
    [(set? x) (set->list x)]
    [else (list x)])) 



;; general way of coercing to vector
(define (->vector x)
  ; todo: on bad input, it will pop a list error rather than vector error
  (cond
    [(vector? x) x]
    [else (list->vector (->list x))]))



;; general way of coercing to boolean
(define (->boolean x)
  ;; in Racket, everything but #f is true
  (if x #t #f))






