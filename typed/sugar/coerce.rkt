#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax))
(require typed/net/url racket/set racket/sequence)
(require typed/sugar/define)
(require "len.rkt") ; want relative path-spec for bilingual conversion

(define-syntax-rule (make-coercion-error-handler target-format x)
  (λ(e) (error (string->symbol (format "->~a" target-format)) (format "Can’t convert ~s to ~a" x target-format))))


(define-type Intable (U Number String Symbol Char Path Lengthable))
(define/typed+provide (->int x)
  (Intable -> Integer)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'int x)])
    (cond
      [(or (integer? x) (real? x)) (assert (inexact->exact (floor x)) integer?)]
      [(complex? x) (->int (real-part x))]
      [(string? x) (let ([strnum (string->number x)])
                     (if (real? strnum) (->int strnum) (error 'ineligible-string)))]
      [(or (symbol? x) (path? x)) (->int (->string x))]
      [(char? x) (char->integer x)]
      [else (len x)]))) ; covers Lengthable types


(provide Stringish)
(define-type Stringish (U String Symbol Number Path Char Null Void))


(define/typed+provide (->string x)
  (Stringish -> String)
  (if (string? x)
      x ; fast exit for strings
      (with-handlers ([exn:fail? (make-coercion-error-handler 'string x)])
        (cond
          [(or (equal? '() x) (void? x)) ""]
          [(symbol? x) (symbol->string x)]
          [(number? x) (number->string x)]
          [(path? x) (path->string x)]
          [(char? x) (format "~a" x)]
          [else (error 'bad-type)]))))


;; ->symbol, ->path, and ->url are just variants on ->string
;; two advantages: return correct type, and more accurate error

;; no need for "Symbolable" type - same as Stringable
(define/typed+provide (->symbol x)
  (Stringish -> Symbol)
  (if (symbol? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'symbol x)])
        (string->symbol (->string x)))))


(define-type Pathish (U Stringish url))
(provide Pathish)
(define/typed+provide (->path x)
  (Pathish -> Path)
  (if (path? x)
      x 
      (with-handlers ([exn:fail? (make-coercion-error-handler 'path x)])
        (cond 
          [(url? x) (apply build-path (cast (map path/param-path (url-path x)) (List* Path-String (Listof Path-String))))]
          [else (string->path (->string x))]))))


;; Use private name here because 'URL' identifier has been added since 6.0
(define-type SugarURL url)
(define/typed+provide (->url x)
  (Stringish -> SugarURL) 
  (with-handlers ([exn:fail? (make-coercion-error-handler 'url x)])
    (string->url (->string x))))


(define/typed+provide (->complete-path x)
  (Stringish -> Path)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'complete-path x)])
    (path->complete-path (->path x))))


(define/typed+provide (->list x)
  (Any -> (Listof Any))
  (if (list? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'list x)])
        (cond 
          [(string? x) (list x)]
          [(vector? x) (for/list ([i (in-vector x)])
                         i)]
          [(set? x) (set->list x)]
          ;; conditional sequencing relevant because hash also tests true for `sequence?`
          [(hash? x) (hash->list x)]
          [(integer? x) (list x)] ; because an integer tests #t for sequence?
          [(sequence? x) (sequence->list x)]
          ;[(stream? x) (stream->list x)] ;; no support for streams in TR
          [else (list x)]))))


(define/typed+provide (->vector x)
  (Any -> VectorTop)
  (if (vector? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'vector x)])
        (list->vector (->list x)))))


(define/typed+provide (->boolean x)
  (Any -> Boolean)
  (and x #t))