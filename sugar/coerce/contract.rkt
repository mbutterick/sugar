#lang racket/base
(require (for-syntax racket/base racket/syntax) racket/contract "../define.rkt" "base.rkt")


(define-syntax-rule (make-blame-handler PROC EXPECTED)
  (λ (b)
    (λ (x) (with-handlers ([exn:fail? (λ (exn)
                                        (raise-blame-error b x
                                         '(expected: "~a" given: "~e")
                                         EXPECTED x))])
             (PROC x)))))


(provide+safe make-coercion-contract)
(define-syntax (make-coercion-contract stx)
  (syntax-case stx ()
    [(_ STEM COERCE-PROC)
     (with-syntax ([COERCE/STEM? (format-id stx "coerce/~a?" #'STEM)]
                   [STEMISH? (format-id stx "~aish?" #'STEM)])
       #'(make-contract
          #:name 'COERCE/STEM?
          #:projection (make-blame-handler COERCE-PROC 'STEMISH?)))]
    [(MACRO-NAME STEM)
     (with-syntax ([->STEM (format-id stx "->~a" #'STEM)])
       #'(MACRO-NAME STEM ->STEM))]))


(define-syntax (define+provide-coercion-contract stx)
  (syntax-case stx ()
    [(_ STEM)
     (with-syntax ([COERCE/STEM? (format-id stx "coerce/~a?" #'STEM)])
       #'(begin
           (provide+safe COERCE/STEM?)
           (define COERCE/STEM? (make-coercion-contract STEM))))]))


(define-syntax-rule (define+provide-coercion-contracts STEM ...)
  (begin (define+provide-coercion-contract STEM) ...))


(define+provide-coercion-contracts int
  string
  symbol
  path
  boolean
  list)
