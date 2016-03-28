#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require "define.rkt")
(provide+safe module-test-external module-test-internal module-test-internal+external)

;; tests using module-boundary contracts
(define-syntax (module-test-external stx)
  (syntax-case stx ()
    [(_ expr ...)
     (let ([mod-name (syntax-e (generate-temporary))])
       (datum->syntax stx
                      `(begin
                         (module* ,mod-name racket/base
                           (require (submod ".."))
                           (require rackunit)
                           ,@(syntax->datum #'(expr ...)))
                         (module+ test
                           (require (submod ".." ,mod-name))))
                      stx))]))

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
