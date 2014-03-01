#lang racket/base
(require (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax (define+provide stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) body ...)
     #'(define+provide proc
         (λ(arg ... . rest-arg) body ...))]
    [(_ name body ...)
     #'(begin
         (provide name)
         (define name body ...))]))
