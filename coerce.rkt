#lang racket/base
(require racket/contract net/url xml racket/set)
(module+ test (require rackunit))
(require "len.rkt")

(provide ->int ->string ->list ->boolean ->symbol ->path)

;; general way of coercing to integer
(define/contract (->int x)
  (any/c . -> . integer?)
  (cond
    [(integer? x) x]
    [(real? x) (floor x)]
    [(and (string? x) (> (len x) 0)) (->int (string->number x))]
    [(symbol? x) (->int (->string x))]
    [(char? x) (char->integer x)]
    [else (or (len x) (error "Can't convert to integer:" x))])) ; try len before giving up


;; general way of coercing to string
(define/contract (->string x)
  (any/c . -> . string?)
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
(define/contract (->path thing)
  (any/c . -> . path?)
  ; todo: on bad input, it will pop a string error rather than path error
  (cond 
    [(url? thing) (apply build-path (map path/param-path (url-path thing)))]
    [else (string->path (->string thing))]))


;; general way of coercing to url
(define/contract (->url thing)
  (any/c . -> . url?)
  ; todo: on bad input, it will pop a string error rather than url error
  (string->url (->string thing)))

(define (->complete-path thing)
  (path->complete-path (->path thing)))


;; general way of coercing to a list
(define/contract (->list x)
  (any/c . -> . list?)
  (cond 
    [(list? x) x]
    [(vector? x) (vector->list x)]
    [(set? x) (set->list x)]
    [else (list x)])) 



;; general way of coercing to vector
(define (->vector x)
  (any/c . -> . vector?)
  ; todo: on bad input, it will pop a list error rather than vector error
  (cond
    [(vector? x) x]
    [else (list->vector (->list x))]))



;; general way of coercing to boolean
(define/contract (->boolean x)
  (any/c . -> . boolean?)
  ;; in Racket, everything but #f is true
  (if x #t #f))






