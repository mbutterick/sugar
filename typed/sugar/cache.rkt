#lang typed/racket/base
(require (for-syntax typed/racket/base) typed/sugar/define)

(define/typed+provide (make-caching-proc base-proc) 
  (All (A B) (A * -> B) -> (A * -> B))
  (let ([cache ((inst make-hash (Listof A) B))])
    (λ args
      (hash-ref! cache args (λ () (apply base-proc args))))))

(provide define/caching)
(define-syntax (define/caching stx)
  (syntax-case stx ()
    [(_ (name arg ... . rest-arg) body ...)
     #'(define/caching name (λ(arg ... . rest-arg) body ...))]
    [(_ name body ...)
     #'(define name (make-caching-proc body ...))]))