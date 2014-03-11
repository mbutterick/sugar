#lang racket/base
(require net/url xml racket/set)
(require "../len.rkt" "../define/provide.rkt")


(define (make-coercion-error-handler target-format x)
  (λ(e) (error (format "Can’t convert ~a to ~a" x target-format))))


(define+provide (->int x)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'integer x)])
    (cond
      [(or (integer? x) (real? x)) (inexact->exact (floor x))] 
      [(and (string? x) (> (len x) 0)) (->int (string->number x))]
      [(symbol? x) (->int (->string x))]
      [(char? x) (char->integer x)]
      [else (len x)])))


(define+provide (->string x)
  (if (string? x)
      x ; fast exit for strings
      (with-handlers ([exn:fail? (make-coercion-error-handler 'string x)])
        (cond
          [(equal? '() x) ""]
          [(symbol? x) (symbol->string x)]
          [(number? x) (number->string x)]
          [(path? x) (path->string x)]
          [(char? x) (format "~a" x)]
          [else (error)]))))


(define+provide (->symbol x)
  (if (symbol? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'symbol x)])
        (string->symbol (->string x)))))


(define+provide (->path x)
  (if (path? x)
      x 
      (with-handlers ([exn:fail? (make-coercion-error-handler 'path x)])
        (cond 
          [(url? x) (apply build-path (map path/param-path (url-path x)))]
          [else (string->path (->string x))]))))


(define+provide (->url x)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'url x)])
    (string->url (->string x))))


(define+provide (->complete-path x)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'complete-path x)])
    (path->complete-path (->path x))))


(define+provide (->list x)
  (if (list? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'list x)])
        (cond 
          [(vector? x) (vector->list x)]
          [(set? x) (set->list x)]
          [else (list x)]))))


(define+provide (->vector x)
  (if (vector? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'vector x)])
        (list->vector (->list x)))))


(define+provide (->boolean x)
  (and x #t))


