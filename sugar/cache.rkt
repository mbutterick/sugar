#lang racket/base
(require (for-syntax racket/base) "define.rkt")


(define+provide+safe (make-caching-proc base-proc) 
  (procedure? . -> . procedure?)
  (let ([cache (make-hash)])
    (make-keyword-procedure
     (λ (kws kw-args . args)
       (hash-ref! cache (list* kws kw-args args) (λ () (keyword-apply base-proc kws kw-args args)))))))


(define-for-syntax (lambdafy stx)
  (syntax-case stx ()
    [(_ (id arg ... . rest-arg) body ...)
     #'(id (λ (arg ... . rest-arg) body ...))]
    [(_ id body-exp)
     #'(id body-exp)]))


(provide+safe define/caching)
(define-syntax (define/caching stx)
  (with-syntax ([(id lambda-expr) (lambdafy stx)])
    #'(define id (make-caching-proc lambda-expr))))
