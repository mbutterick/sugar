#lang racket/base
(require (for-syntax racket/base) "define.rkt")

(define+provide+safe (make-caching-proc base-proc) 
  (procedure? . -> . procedure?)
  (let ([cache (make-hash)])
    (make-keyword-procedure
     (λ (kws kw-args . args)
       (hash-ref! cache args (λ () (keyword-apply base-proc kws kw-args args)))))))

(provide+safe define/caching)
(define-syntax (define/caching stx)
  (syntax-case stx ()
    [(_ (name arg ... . rest-arg) body ...)
     #'(define/caching name (λ(arg ... . rest-arg) body ...))]
    [(_ name body ...)
     #'(define name (make-caching-proc body ...))]))