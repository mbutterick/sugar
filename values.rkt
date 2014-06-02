#lang racket/base
(require (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))


(define-syntax (apply-values stx)
  (syntax-case stx ()
    [(_ proc values) #'(apply proc (values->list values))]))
