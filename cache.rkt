#lang racket/base
(require (for-syntax racket/base) racket/contract)
(provide (all-defined-out))

(define/contract (make-caching-proc base-proc) 
  (procedure? . -> . procedure?)
  (let ([cache (make-hash)])
    (λ args
      (hash-ref! cache args (λ () (apply base-proc args))))))

(define-syntax (define/caching stx)
  (syntax-case stx ()
    [(_ (name arg ... . rest-arg) body ...)
     #'(define/caching name (λ(arg ... . rest-arg) body ...))]
    [(_ name body ...)
     #'(define name (make-caching-proc body ...))]))