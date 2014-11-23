#lang racket/base
(require (for-syntax racket/base racket/syntax))

(provide (all-defined-out))

(define-syntax (report stx)
  (syntax-case stx ()
    [(_ expr) #'(report expr expr)]
    [(_ expr name)
     #'(begin 
         (displayln (format "~a = ~v" 'name expr) (current-error-port)) 
         expr)]))


(define-syntax (report* stx)
  (syntax-case stx ()
    [(_ expr ...) (datum->syntax stx `(begin ,@(map (λ(arg) `(report ,arg)) (syntax->datum #'(expr ...)))))]))


(define-syntax-rule (repeat num expr ...)
  (for/last ([i (in-range num)])
    expr ...))


(define-syntax-rule (time-repeat num expr ...)
  (time (repeat num expr ...)))


(define-syntax (time-repeat* stx)
  (syntax-case stx ()
    [(_ num expr ...) 
     (let ([num (syntax->datum #'num)])
       (datum->syntax stx `(values ,@(map (λ(arg) `(time-repeat ,num ,arg)) (syntax->datum #'(expr ...))))))]))


(define-syntax (compare stx)
  (syntax-case stx ()
    [(_ expr id id-alts ...) 
     (let ([expr (syntax->datum #'expr)]
           [id (syntax->datum #'id)])
       (datum->syntax stx `(values ,expr ,@(map (λ(id-alt) `(let ([,id ,id-alt]) ,expr)) (syntax->datum #'(id-alts ...))))))]))
