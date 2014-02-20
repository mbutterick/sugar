#lang racket/base
(require racket/contract net/url xml racket/set)
(module+ test (require rackunit))
(require "len.rkt" "exception.rkt" "define.rkt" "debug.rkt")


(define (make-coercion-error-handler target-format x)
  (λ(e) (error (format "Can’t convert ~a to ~a" x target-format))))

;; general way of coercing to integer
(define+provide/contract (->int x)
  (any/c . -> . integer?)
  (try 
   (cond
     [(or (integer? x) (real? x)) (inexact->exact (floor x))] 
     [(and (string? x) (> (len x) 0)) (->int (string->number x))]
     [(symbol? x) (->int (->string x))]
     [(char? x) (char->integer x)]
     [else (len x)])
   (except [exn:fail? (make-coercion-error-handler 'integer x)])))



;; general way of coercing to string
(define+provide/contract (->string x)
  (any/c . -> . string?)
  (if (string? x)
      x ; fast exit for strings
      (try   
       (cond
         [(equal? '() x) ""]
         [(symbol? x) (symbol->string x)]
         [(number? x) (number->string x)]
         [(url? x) (->string (->path x))] ; todo: a url is more than just a path-string ... it has character encoding issues
         [(path? x) (path->string x)]
         [(char? x) (format "~a" x)]
         [else (error)]) ; put this last so other xexprish things don't get caught
       (except [exn:fail? (make-coercion-error-handler 'string x)]))))


;; general way of coercing to html
(define+provide/contract (->html x)
  (any/c . -> . string?)
  (try (xexpr->string x)
       (except [exn:fail? (make-coercion-error-handler 'html x)])))


;; general way of coercing to symbol
(define+provide/contract (->symbol x)
  (any/c . -> . symbol?)
  (try (string->symbol (->string x)) 
       (except [exn:fail? (make-coercion-error-handler 'symbol x)])))

;; general way of coercing to path
(define+provide/contract (->path x)
  (any/c . -> . path?)
  (try
   (cond 
     [(url? x) (apply build-path (map path/param-path (url-path x)))]
     [else (string->path (->string x))])
   (except [exn:fail? (make-coercion-error-handler 'path x)])))


;; general way of coercing to url
(define+provide/contract (->url x)
  (any/c . -> . url?)
  (try (string->url (->string x))
       (except [exn:fail? (make-coercion-error-handler 'url x)])))

(define+provide/contract (->complete-path x)
  (any/c . -> . complete-path?)
  (try (path->complete-path (->path x))
       (except [exn:fail? (make-coercion-error-handler 'complete-path x)])))


;; general way of coercing to a list
(define+provide/contract (->list x)
  (any/c . -> . list?)
  (try
   (cond 
     [(list? x) x]
     [(vector? x) (vector->list x)]
     [(set? x) (set->list x)]
     [else (list x)])
   (except [exn:fail? (make-coercion-error-handler 'list x)])))


;; general way of coercing to vector
(define+provide/contract (->vector x)
  (any/c . -> . vector?)
  (try
   (cond
     [(vector? x) x]
     [else (list->vector (->list x))])
   (except [exn:fail? (make-coercion-error-handler 'vector x)])))



;; general way of coercing to boolean
(define+provide/contract (->boolean x)
  (any/c . -> . boolean?)
  (if x #t #f))


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


(define+provide coerce/integer?
  (make-contract
   #:name 'coerce/integer?
   #:projection (make-blame-handler ->int 'can-be-integer?)))

(define+provide coerce/string?
  (make-contract
   #:name 'coerce/string?
   #:projection (make-blame-handler ->string 'can-be-string?)))

(define+provide coerce/symbol?
  (make-contract
   #:name 'coerce/symbol?
   #:projection (make-blame-handler ->symbol 'can-be-symbol?)))

(define+provide coerce/path?
  (make-contract
   #:name 'coerce/path?
   #:projection (make-blame-handler ->path 'can-be-path?)))

(define+provide coerce/boolean?
  (make-contract
   #:name 'coerce/boolean?
   #:projection (make-blame-handler ->boolean 'can-be-boolean?)))


