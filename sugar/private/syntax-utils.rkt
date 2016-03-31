#lang racket/base
(require (for-syntax racket/base) syntax/define)
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
;; hoist contract out of lambda-exp entirely
(define-syntax-rule (lambdafy-with-contract stx)
  (syntax-case stx ()
    [(_ id contract lambda-exp)
     ;; `normalize-definition` can't handle the `define/contract` pattern of three expressions after the `define`.
     ;; so extract the contract, and then put id & lambda-exp back together, and let `normalize-definition` destructure as usual.
     (with-syntax ([(new-id new-lambda-exp) (let-values ([(id-stx body-exp-stx) (normalize-definition #'(_ id lambda-exp) (datum->syntax stx 'λ) #t #t)])
                                      (list id-stx body-exp-stx))])
       #'(new-id contract new-lambda-exp))]
    [else
     (with-syntax ([(id (lambda args contract body-exp (... ...))) (let*-values ([(id-stx rhs-exp-stx) (normalize-definition stx (datum->syntax stx 'λ) #t #t)])
                                                                     (list id-stx (syntax->list rhs-exp-stx)))])
       ;; lambda-exp = #'(lambda args body-exp (... ...))
       #'(id contract (lambda args body-exp (... ...))))]))


;; convert calling pattern to form (id body-exp)
(define-syntax-rule (lambdafy stx)
  (with-syntax ([(id lambda-exp) (let-values ([(id-stx body-exp-stx) (normalize-definition stx (datum->syntax stx 'λ) #t #t)])
                                   (list id-stx body-exp-stx))])
    #'(id lambda-exp)))