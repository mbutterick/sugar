#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))

(provide (all-defined-out))

(define-syntax (report stx)
  (syntax-case stx ()
    [(_ expr) #'(report expr expr)]
    [(_ expr name)
     #'(let ([x expr]) 
         (displayln (format "~a = ~v" 'name x) (current-error-port)) 
         x)]))

(define-syntax-rule (report-apply proc expr) 
  (let ([lst expr])
    (report (apply proc lst) (apply proc expr)) 
    lst))

#|
(define-syntax (verbalize stx)
  (syntax-case stx ()
    [(_ proc args ...)
     (with-syntax ([proc-input (format-id stx "args to ~a" #'proc)])
     #'(begin
         (let () (report (list args ...) proc-input) (void))
         (report (proc args ...))))]))
|#

(define-syntax (report* stx)
  (syntax-case stx ()
    [(_ expr ...) #'(begin (report expr) ...)]))


(define-syntax-rule (repeat num expr ...)
  (for/last ([i (in-range num)])
    expr ...))


(define-syntax-rule (time-repeat num expr ...)
  (time (repeat num expr ...)))


(define-syntax (time-repeat* stx)
  (syntax-case stx ()
    [(_ num expr ...) 
     #'(let ([n num])
         (values (time-repeat n expr) ...))]))


(define-syntax (compare stx)
  (syntax-case stx ()
    [(_ expr id id-alt ...) 
     #'(values expr (let ([id id-alt]) expr) ...)]))
