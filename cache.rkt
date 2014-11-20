#lang racket/base
(require (for-syntax racket/base))
(provide (all-defined-out))

(define (make-caching-proc base-proc) 
  (let ([cache (make-hash)])
    (λ args
      (hash-ref! cache args (λ () (apply base-proc args))))))

(define-syntax (define/caching stx)
  (syntax-case stx ()
    [(_ (name arg ... . rest-arg) body ...)
     #'(define/caching name (λ(arg ... . rest-arg) body ...))]
    [(_ name body ...)
     #'(define name (make-caching-proc body ...))]))