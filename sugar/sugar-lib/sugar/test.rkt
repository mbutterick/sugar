#lang racket/base
(require (for-syntax
          racket/base
          racket/syntax
          syntax/strip-context)
         "define.rkt")

(provide+safe module-test-external
              module-test-internal
              module-test-internal+external)

;; tests using module-boundary contracts
(define-syntax (module-test-external stx)
  (syntax-case stx ()
    [(_ EXPR ...)
     (replace-context
      stx
      (with-syntax ([MOD-NAME (syntax-e (generate-temporary))])
        #'(begin
            (module* MOD-NAME racket/base
              (require (submod ".."))
              (require rackunit)
              EXPR ...)
            (module+ test
              (require (submod ".." MOD-NAME))))))]))

(define-syntax (module-test-internal stx)
  (syntax-case stx ()
    [(_ EXPR ...)
     (replace-context
      stx
      #'(begin
          (module+ test
            (require rackunit)
            EXPR ...)))]))

(define-syntax (module-test-internal+external stx)
  (syntax-case stx ()
    [(_ EXPR ...)
     (replace-context
      stx
      #'(begin
          (module-test-internal EXPR ...)
          (module-test-external EXPR ...)))]))
