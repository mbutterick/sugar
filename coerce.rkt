#lang racket/base
(require racket/contract net/url xml racket/set)
(module+ test (require rackunit))
(require "len.rkt" "exception.rkt" "define.rkt")

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
  (try 
   (cond
     [(or (integer? x) (real? x)) (inexact->exact (floor x))] 
     [(and (string? x) (> (len x) 0)) (->int (string->number x))]
     [(symbol? x) (->int (->string x))]
     [(char? x) (char->integer x)]
     [else (len x)])
   (except [exn:fail? (λ(e) (error "Can't convert to integer:" x))])))



;; general way of coercing to string
(define (->string x)
  (try 
   (cond 
     [(string? x) x]
     [(equal? '() x) ""]
     [(symbol? x) (symbol->string x)]
     [(number? x) (number->string x)]
     [(url? x) (->string (->path x))] ; todo: a url is more than just a path-string ... it has character encoding issues
     [(path? x) (path->string x)]
     [(char? x) (format "~a" x)]
     [else (error)]) ; put this last so other xexprish things don't get caught
   (except [exn:fail? (λ(e) (error (format "Can't make ~a into string" x)))])))


;; general way of coercing to symbol
(define (->symbol x)
  (try (string->symbol (->string x)) 
       (except [exn:fail? (λ(e) (error (format "Can't make ~a into symbol" x)))])))

;; general way of coercing to path
(define (->path x)
  (try
   (cond 
     [(url? x) (apply build-path (map path/param-path (url-path x)))]
     [else (string->path (->string x))])
   (except [exn:fail? (λ(e) (error (format "Can't make ~a into path" x)))])))


;; general way of coercing to url
(define (->url x)
  (try (string->url (->string x))
       (except [exn:fail? (λ(e) (error (format "Can't make ~a into url" x)))])))

(define (->complete-path x)
  (try (path->complete-path (->path x))
       (except [exn:fail? (λ(e) (error (format "Can't make ~a into complete-path" x)))])))


;; general way of coercing to a list
(define (->list x)
  (try
   (cond 
     [(list? x) x]
     [(vector? x) (vector->list x)]
     [(set? x) (set->list x)]
     [else (list x)])
   (except [exn:fail? (λ(e) (error (format "Can't make ~a into list" x)))])))


;; general way of coercing to vector
(define (->vector x)
  (try
   (cond
     [(vector? x) x]
     [else (list->vector (->list x))])
   (except [exn:fail? (λ(e) (error (format "Can't make ~a into vector" x)))])))



;; general way of coercing to boolean
(define (->boolean x)
  (try
   (if x #t #f)
   (except [exn:fail? (λ(e) (error (format "Can't make ~a into boolean" x)))])))


;;
;; Coercion contracts
;;

(define-syntax-rule (make-blame-handler try-proc expected-sym)
  (λ (b)
    (λ (x)
      (try (try-proc x)
           (except [exn:fail? (λ(e)
                                (raise-blame-error
                                 b x
                                 '(expected: "~a" given: "~e")
                                 expected-sym x))])))))


(define/provide coerce/integer?
  (make-contract
   #:name 'coerce/integer?
   #:projection (make-blame-handler ->int 'can-be-integer?)))

(define/provide coerce/string?
  (make-contract
   #:name 'coerce/string?
   #:projection (make-blame-handler ->string 'can-be-string?)))

(define/provide coerce/symbol?
  (make-contract
   #:name 'coerce/symbol?
   #:projection (make-blame-handler ->symbol 'can-be-symbol?)))

(define/provide coerce/path?
  (make-contract
   #:name 'coerce/path?
   #:projection (make-blame-handler ->path 'can-be-path?)))

(define/provide coerce/boolean?
  (make-contract
   #:name 'coerce/boolean?
   #:projection (make-blame-handler ->boolean 'can-be-boolean?)))

