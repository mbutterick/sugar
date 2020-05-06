#lang racket/base
(require (for-syntax
          racket/base
          sugar/private/syntax-utils)
         "define.rkt")

(define+provide+safe (make-caching-proc base-proc) 
  (procedure? . -> . procedure?)
  (let ([cache (make-hash)])
    (make-keyword-procedure
     (λ (kws kw-args . args)
       (hash-ref! cache (list* kws kw-args args) (λ () (keyword-apply base-proc kws kw-args args)))))))

(provide+safe define/caching)
(define-syntax (define/caching stx)
  (with-syntax ([(ID LAMBDA-EXPR) (lambdafy stx)])
    #'(define ID (make-caching-proc LAMBDA-EXPR))))
