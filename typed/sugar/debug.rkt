#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))
(provide (all-defined-out))


(define-syntax (report stx)
  (syntax-case stx ()
    [(_ expr) #'(report expr expr)]
    [(_ expr name)
     #'(let ([expr-result expr]) 
         (eprintf "~a = ~v\n" 'name expr-result)
         expr-result)]))


(define-syntax (report/line stx)
  (syntax-case stx ()
    [(_ expr) #'(report/line expr expr)]
    [(_ expr name)
     (with-syntax ([line (syntax-line #'expr)])
       #'(let ([expr-result expr])
           (eprintf "~a = ~v on line ~v\n" 'name expr-result line)
           expr-result))]))


(define-syntax (report/file stx)
  (syntax-case stx ()
    [(_ expr) #'(report/file expr expr)]
    [(_ expr name)
     (with-syntax ([file (syntax-source #'expr)]
                   [line (syntax-line #'expr)])
       #'(let ([expr-result expr])
           (eprintf "~a = ~v on line ~v in \"~a\"\n" 'name expr-result line file)
           expr-result))]))


(define-syntax-rule (define-multi-version multi-name name)
  (define-syntax-rule (multi-name x (... ...))
    (begin (name x) (... ...))))

(define-multi-version report* report)
(define-multi-version report*/line report/line)
(define-multi-version report*/file report/file)


(define-syntax report-apply
  (syntax-rules ()
    [(report-apply proc expr) 
     (let ([lst expr])
       (report (apply proc lst) (apply proc expr)) 
       lst)]
    [(report-apply proc expr #:line)
     (let ([lst expr])
       (report (apply proc lst) (apply proc expr) #:line)
       lst)]))

#|
(define-syntax (verbalize stx)
  (syntax-case stx ()
    [(_ proc args ...)
     (with-syntax ([proc-input (format-id stx "args to ~a" #'proc)])
     #'(begin
         (let () (report (list args ...) proc-input) (void))
         (report (proc args ...))))]))
|#




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
