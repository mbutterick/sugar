#lang racket/base
(require (for-syntax racket/base racket/syntax))

(provide report)

(define-syntax (report stx)
  (syntax-case stx ()
    [(_ expr) #'(report expr expr)]
    [(_ expr name)
     #'(begin 
         (displayln (format "~a = ~v" 'name expr) (current-error-port)) 
         expr)]))