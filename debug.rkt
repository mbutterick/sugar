#lang racket/base
(require (for-syntax racket/base racket/syntax))

(provide report report*)

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
