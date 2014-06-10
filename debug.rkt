#lang racket/base

(provide report)

(define-syntax-rule (report var)
  (begin 
    (displayln (format "~a = ~v" 'var var) (current-error-port)) 
    var))
