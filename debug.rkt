#lang racket/base

(provide report)

(define-syntax-rule (report expr)
  (begin 
    (displayln (format "~a = ~v" 'expr expr) (current-error-port)) 
    expr))
