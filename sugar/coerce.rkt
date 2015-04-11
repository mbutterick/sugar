#lang racket/base
(require (for-syntax racket/base racket/syntax) sugar/define net/url)

(require-via-wormhole "../typed/sugar/coerce.rkt")


(provide+safe [->int (any/c . -> . integer?)]
              [->string (any/c . -> . string?)]
              [->symbol (any/c . -> . symbol?)]
              [->path (any/c . -> . path?)]
              [->complete-path (any/c . -> . complete-path?)]
              [->url (any/c . -> . url?)]
              [->list (any/c . -> . list?)]
              [->vector (any/c . -> . vector?)]
              [->boolean (any/c . -> . boolean?)])


;; coercion contracts & *ish predicates
;; only make sense in untyped code
;; thus they are here.
(define-syntax-rule (make-blame-handler try-proc expected-sym)
  (λ(b)
    (λ(x)
      (with-handlers ([exn:fail? (λ(e)
                                   (raise-blame-error
                                    b x
                                    '(expected: "~a" given: "~e")
                                    expected-sym x))])
        (try-proc x)))))

(provide+safe make-coercion-contract)
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
       #'(begin
           (provide+safe coerce/stem?)
           (define coerce/stem? (make-coercion-contract stem))))]))

(define+provide-coercion-contract int)
(define+provide-coercion-contract string)
(define+provide-coercion-contract symbol)
(define+provide-coercion-contract path)
(define+provide-coercion-contract boolean)
(define+provide-coercion-contract list)



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