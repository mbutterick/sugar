#lang racket/base
(require (for-syntax racket/base))
(provide (all-defined-out))


;; tests using module-boundary contracts
(define-syntax (module-test-external stx)
  (syntax-case stx ()
    [(_ expr ...)
     #'(begin
         (module* test-external racket/base
           (require (submod ".."))
           (require rackunit)
           expr ...)
         (module+ test
           (require (submod ".." test-external))))]))


(define-syntax (module-test-internal stx)
  (syntax-case stx ()
    [(_ expr ...)
     (let ([exprs (syntax->datum #'(expr ...))])
       (datum->syntax stx `(begin
                             (module+ test
                               (require rackunit)
                               ,@exprs))
                      ;; pass original stx for srcloc
                      ;; which is not precisely accurate but
                      ;; OK for now
                      stx))]))

(define-syntax (module-test-internal+external stx)
  (syntax-case stx ()
    [(_ expr ...)
     (let ([exprs (syntax->datum #'(expr ...))])
       (datum->syntax stx `(begin
                             (module-test-internal ,@exprs)
                             (module-test-external ,@exprs)) stx))]))