#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require net/url racket/sequence "../unstable/len.rkt" "../define.rkt")

(define-syntax-rule (make-coercion-error-handler target-format x)
  (λ(e) (error (string->symbol (format "->~a" target-format)) (format "Can't convert ~s to ~a" x target-format))))


(define+provide+safe (->int x)
  (any/c . -> . integer?)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'int x)])
    (cond
      [(or (integer? x) (real? x)) (inexact->exact (floor x))]
      [(complex? x) (->int (real-part x))]
      [(string? x) (let ([strnum (string->number x)])
                     (if (real? strnum) (->int strnum) (error 'ineligible-string)))]
      [(or (symbol? x) (path? x) (bytes? x)) (->int (->string x))]
      [(char? x) (char->integer x)]
      [else (len x)]))) ; covers Lengthable types


(define+provide+safe (->string x)
  (any/c . -> . string?)
  (if (string? x)
      x ; fast exit for strings
      (with-handlers ([exn:fail? (make-coercion-error-handler 'string x)])
        (cond
          [(or (null? x) (void? x)) ""]
          [(symbol? x) (symbol->string x)]
          [(number? x) (number->string x)]
          [(path? x) (path->string x)]
          [(or (char? x) (bytes? x)) (format "~a" x)]
          [(url? x) (url->string x)]
          [else (error 'bad-type)]))))


;; ->symbol, ->path, and ->url are just variants on ->string
;; two advantages: return correct type, and more accurate error

;; no need for "Symbolable" type - same as Stringable
(define+provide+safe (->symbol x)
  (any/c . -> . symbol?)
  (if (symbol? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'symbol x)])
        (string->symbol (->string x)))))


(define+provide+safe (->path x)
  (any/c . -> . path?)
  (if (path? x)
      x 
      (with-handlers ([exn:fail? (make-coercion-error-handler 'path x)])
        (cond 
          [(url? x) (apply build-path (map path/param-path (url-path x)))]
          [else (string->path (->string x))]))))


(define+provide+safe (->url x)
  (any/c . -> . url?) 
  (with-handlers ([exn:fail? (make-coercion-error-handler 'url x)])
    (string->url (->string x))))


(define+provide+safe (->complete-path x)
  (any/c . -> . complete-path?)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'complete-path x)])
    (path->complete-path (->path x))))


(define+provide+safe (->list x)
  (any/c . -> . list?)
  (if (list? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'list x)])
        (cond 
          [(string? x) (list x)]
          [(vector? x) (for/list ([i (in-vector x)])
                         i)]
          ;; conditional sequencing relevant because hash also tests true for `sequence?`
          [(hash? x) (hash->list x)]
          [(integer? x) (list x)] ; because an integer tests #t for sequence?
          [(sequence? x) (sequence->list x)]
          ;[(stream? x) (stream->list x)] ;; no support for streams in TR
          [else (list x)]))))


(define+provide+safe (->vector x)
  (any/c . -> . vector?)
  (if (vector? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'vector x)])
        (list->vector (->list x)))))


(define+provide+safe (->boolean x)
  (any/c . -> . boolean?)
  (and x #t))


(define-syntax (make-*ish-predicate stx)
  (syntax-case stx ()
    [(_ stem)
     (with-syntax ([stemish? (format-id stx "~aish?" #'stem)]
                   [->stem (format-id stx "->~a" #'stem)])
       #`(begin
           (provide+safe stemish?)
           (define (stemish? x)
             (with-handlers ([exn:fail? (λ(e) #f)]) (and (->stem x) #t)))))]))


(make-*ish-predicate int)
(make-*ish-predicate string)
(make-*ish-predicate symbol)
(make-*ish-predicate url)
(make-*ish-predicate complete-path)
(make-*ish-predicate path)
(make-*ish-predicate list)
(make-*ish-predicate vector)