#lang racket/base
(require (for-syntax racket/base) syntax/strip-context)
(provide (all-defined-out))


(define-syntax-rule (require+provide/safe modname ...)
  (begin
    (begin
      (require modname)
      (provide (all-from-out modname))
      (module+ safe
        (require (submod modname safe))
        (provide (all-from-out (submod modname safe))))) ...))


;; convert calling pattern to form (id contract body-exp)
(define-syntax-rule (lambdafy-with-contract stx)
  (syntax-case stx ()
    [(_ (id arg (... ...) . rest-arg) contract body0 body (... ...))
     (replace-context #'id  #'(id contract (λ (arg (... ...) . rest-arg) body0 body (... ...))))]
    [(_ id contract lambda-exp)
     (replace-context #'id #'(id contract lambda-exp))]))


;; convert calling pattern to form (id body-exp)
(define-syntax-rule (lambdafy stx)
  (syntax-case stx ()
    [(_ (id arg (... ...) . rest-arg) body0 body (... ...))
     (replace-context #'id #'(id (λ (arg (... ...) . rest-arg) body0 body (... ...))))]
    [(_ id lambda-exp)
     (replace-context #'id #'(id lambda-exp))]))