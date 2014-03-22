#lang racket/base
(require (for-syntax racket/base))
(require racket/syntax)

(provide define->macro)


(define-syntax define->macro  
  (syntax-rules ()
    [(_ (proc-name arg ... . rest-arg) proc-expr ...) (define->macro proc-name (λ(arg ... . rest-arg) proc-expr ...))]
    [(_ proc-name proc) (define-syntax proc-name 
                          (syntax-id-rules ()
                            [(proc-name expr (... ...)) (proc expr (... ...))]
                            [proc-name proc]))]))


;(define foo (λ(x) (add1 x)))
(define->macro foo (λ(x) (add1 x)))

(foo 4)
(map foo '(2 4 6))

;(define (bar x) (add1 x))
(define->macro (bar x y) (+ x y))

(bar 4 1)
(map bar '(2 4 6) '(2 4 6))
