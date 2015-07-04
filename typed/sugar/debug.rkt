#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))

(provide (all-defined-out))

(define-syntax (report stx)
  (syntax-case stx ()
    [(_ expr) #'(report expr expr)]
    [(_ expr #:line) #'(report expr expr #:line)]
    [(_ expr name)
     #'(let ([x expr]) 
         (eprintf "~a = ~v\n" 'name x)
         x)]
    [(_ expr name #:line)
     (with-syntax ([line (syntax-line #'expr)])
       #'(let ([x expr])
           (eprintf "~a = ~v on line ~v\n" 'name x line)
           x))]
    ))

(define-syntax report-apply
  (syntax-rules ()
    [(report-apply proc expr) 
     (let ([lst expr])
       (report (apply proc lst) (apply proc expr)) 
       lst)]
    [(report-apply proc expr #:line)
     (let ([lst expr])
       (report (apply proc lst) (apply proc expr) #:line)
       lst)]
    ))

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
