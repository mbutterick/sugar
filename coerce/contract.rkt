#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/contract "../define/provide.rkt" "value.rkt")



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
    [(_ stem)
     (let ([stem-datum (syntax->datum #'stem)])
       (with-syntax ([coerce/stem? (format-id stx "coerce/~a?" #'stem)]
                     [->stem (format-id stx "->~a" #'stem)]
                     [can-be-stem? (format-id stx "can-be-~a?" #'stem)])
         #'(define+provide coerce/stem?
             (make-contract
              #:name 'coerce/stem?
              #:projection (make-blame-handler ->stem 'can-be-stem?)))))]))


(make-coercion-contract int)
(make-coercion-contract string)
(make-coercion-contract symbol)
(make-coercion-contract path)
(make-coercion-contract boolean)

#|
(define/contract (foo x)
  (coerce/string? . -> . any/c)
  x)|#