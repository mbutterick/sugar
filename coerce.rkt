#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require net/url racket/set racket/contract racket/sequence racket/stream racket/dict)
(require "len.rkt" "define.rkt")

(define (make-coercion-error-handler target-format x)
  (λ(e) (error (format "Can’t convert ~a to ~a" x target-format))))


(define+provide (->int x)
  (with-handlers ([exn:fail? (make-coercion-error-handler 'integer x)])
    (cond
      [(or (integer? x) (real? x)) (inexact->exact (floor x))] 
      [(and (string? x) (> (len x) 0)) (->int (string->number x))]
      [(symbol? x) (->int (->string x))]
      [(char? x) (char->integer x)]
      [(path? x) (->int (->string x))]
      [else (len x)])))


(provide ->macrostring)
(define-syntax-rule (->macrostring x)
  (if (string? x)
      x ; fast exit for strings
      (with-handlers ([exn:fail? (make-coercion-error-handler 'string (format "~a (result of ~a" x 'x))])
        (cond
          [(or (equal? '() x) (void? x)) ""]
          [(symbol? x) (symbol->string x)]
          [(number? x) (number->string x)]
          [(path? x) (path->string x)]
          [(char? x) (format "~a" x)]
          [else (error)]))))

(define+provide (->string x)
  (if (string? x)
      x ; fast exit for strings
      (with-handlers ([exn:fail? (make-coercion-error-handler 'string x)])
        (cond
          [(or (equal? '() x) (void? x)) ""]
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
          [(string? x) (list x)]
          [(vector? x) (vector->list x)]
          [(set? x) (set->list x)]
          ;; location relevant because hash or dict are also sequences
          [(dict? x) (dict->list x)] 
          [(integer? x) (list x)] ; because an integer tests #t for sequence?
          [(sequence? x) (sequence->list x)]
          [(stream? x) (stream->list x)]
          [else (list x)]))))


(define+provide (->vector x)
  (if (vector? x)
      x
      (with-handlers ([exn:fail? (make-coercion-error-handler 'vector x)])
        (list->vector (->list x)))))


(define+provide (->boolean x)
  (and x #t))


(define-syntax (make-*ish-predicate stx)
  (syntax-case stx ()
    [(_ stem)
     (with-syntax ([stemish? (format-id stx "~aish?" #'stem)]
                   [->stem (format-id stx "->~a" #'stem)])
       #`(begin
           (define+provide (stemish? x)
             (with-handlers ([exn:fail? (λ(e) #f)]) (and (->stem x) #t)))))]))

(make-*ish-predicate int)
(make-*ish-predicate string)
(make-*ish-predicate symbol)
(make-*ish-predicate url)
(make-*ish-predicate complete-path)
(make-*ish-predicate path)
(make-*ish-predicate list)
(make-*ish-predicate vector)
;; no point to having list and vector here; they work with everything



(define-syntax-rule (make-blame-handler try-proc expected-sym)
  (λ(b)
    (λ(x)
      (with-handlers ([exn:fail? (λ(e)
                                   (raise-blame-error
                                    b x
                                    '(expected: "~a" given: "~e")
                                    expected-sym x))])
        (try-proc x)))))

(provide make-coercion-contract)
(define-syntax (make-coercion-contract stx)
  (syntax-case stx ()
    [(_ stem coerce-proc)
     (with-syntax ([coerce/stem? (format-id stx "coerce/~a?" #'stem)]
                   [can-be-stem? (format-id stx "can-be-~a?" #'stem)])
       #'(make-contract
          #:name 'coerce/stem?
          #:projection (make-blame-handler coerce-proc 'can-be-stem?)))]
    [(_ stem)
     (with-syntax ([->stem (format-id stx "->~a" #'stem)])
       #'(make-coercion-contract stem ->stem))]))

(define-syntax (define+provide-coercion-contract stx)
  (syntax-case stx ()
    [(_ stem)
     (with-syntax ([coerce/stem? (format-id stx "coerce/~a?" #'stem)])
       #'(define+provide coerce/stem? (make-coercion-contract stem)))]))

(define+provide-coercion-contract int)
(define+provide-coercion-contract string)
(define+provide-coercion-contract symbol)
(define+provide-coercion-contract path)
(define+provide-coercion-contract boolean)
(define+provide-coercion-contract list)

