#lang racket/base
(require (for-syntax
          racket/base
          racket/syntax)
         racket/stream
         racket/generic
         net/url
         racket/sequence
         "../unstable/len.rkt"
         "../define.rkt")

(module+ safe (require racket/contract))

(define-syntax-rule (make-coercion-error-handler func funcish val)
  (λ (exn) (raise-argument-error 'func (symbol->string 'funcish) val)))

(define (disjoin . preds) (λ (x) (ormap (λ (pred) (pred x)) preds)))
(define identity (λ (x) x))

(define-generics+provide+safe stringish
  (any/c . -> . string?)
  (->string stringish)
  #:fast-defaults
  ([string? (define ->string identity)]
   [(disjoin null? void?) (define (->string x) "")]
   [symbol? (define ->string symbol->string)]
   [number? (define ->string number->string)]
   [path? (define ->string path->string)]
   [(disjoin char? bytes?) (define (->string x) (format "~a" x))]
   [url? (define ->string url->string)]))


(define (real->int x) (inexact->exact (floor x)))
(define (string->int x) (let ([strnum (string->number x)])
                          (unless (real? strnum)
                            (raise-argument-error '->int "eligible string" x))
                          (real->int strnum)))

(define-generics+provide+safe intish
  (any/c . -> . integer?)
  (->int intish)
  #:fast-defaults
  ([(disjoin integer? real?) (define ->int real->int)]
   [complex? (define ->int (compose1 real->int real-part))]
   [string? (define ->int string->int)]
   [(disjoin symbol? path? bytes?) (define ->int (compose1 string->int ->string))]
   [char? (define ->int char->integer)]
   [lengthable? (define (->int x)
                  (with-handlers ([exn:fail? (make-coercion-error-handler ->int intish? x)])
                    (len x)))]))


(define-generics+provide+safe symbolish
  (any/c . -> . symbol?)
  (->symbol symbolish)
  #:fast-defaults
  ([symbol? (define ->symbol identity)]
   [stringish? (define (->symbol x)
                 (with-handlers ([exn:fail? (make-coercion-error-handler ->symbol symbolish? x)])
                   (string->symbol (->string x))))]))


(define-generics+provide+safe pathish
  (any/c . -> . path?)
  (->path pathish)
  #:fast-defaults
  ([path? (define ->path identity)]
   [stringish? (define (->path x)
                 (with-handlers ([exn:fail? (make-coercion-error-handler ->path pathish? x)])
                   (if (url? x)
                       (apply build-path (map path/param-path (url-path x)))
                       (string->path (->string x)))))]))


(define-generics+provide+safe urlish
  (any/c . -> . url?)
  (->url urlish)
  #:fast-defaults
  ([url? (define ->url identity)]
   [stringish? (define (->url x)
                 (with-handlers ([exn:fail? (make-coercion-error-handler ->url urlish? x)])
                   (string->url (->string x))))]))


(define-generics+provide+safe complete-pathish
  (any/c . -> . complete-path?)
  (->complete-path complete-pathish)
  #:fast-defaults
  ([complete-path? (define ->complete-path identity)]
   [stringish? (define (->complete-path x)
                 (with-handlers ([exn:fail? (make-coercion-error-handler ->complete-path complete-pathish? x)])
                   (path->complete-path (->path x))))]))


(define-generics+provide+safe listish
  (any/c . -> . list?)
  (->list listish)
  #:fast-defaults
  ([list? (define ->list identity)]
   [string? (define ->list list)]
   [vector? (define ->list vector->list)]
   [hash? (define ->list hash->list)]
   [integer? (define ->list list)]
   [sequence? (define ->list sequence->list)]
   [stream? (define ->list stream->list)]
   [(λ (x) #t) (define ->list list)]))


(define-generics+provide+safe vectorish
  (any/c . -> . vector?)
  (->vector vectorish)
  #:fast-defaults
  ([vector? (define ->vector identity)]
   [listish? (define (->vector x)
               (with-handlers ([exn:fail? (make-coercion-error-handler ->vector vectorish? x)])
                 (list->vector (->list x))))]))


(define+provide+safe (->boolean x)
  (any/c . -> . boolean?)
  (and x #t))