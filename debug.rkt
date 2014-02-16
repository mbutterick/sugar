#lang racket/base

(provide report describe)

; report the current value of the variable, then return it
(define-syntax-rule (report var)
  (begin 
    (displayln (format "~a = ~a" 'var var) (current-error-port)) 
    var))


(require (prefix-in williams: (planet williams/describe/describe)))

(define (describe x)
  (parameterize ([current-output-port (current-error-port)])
    (williams:describe x))
  x)