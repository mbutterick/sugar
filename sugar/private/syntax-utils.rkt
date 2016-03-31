#lang racket/base
(require (for-syntax racket/base) syntax/define)
(provide (except-out (all-defined-out) values->list))


(define-syntax-rule (require+provide/safe modname ...)
  (begin
    (begin
      (require modname)
      (provide (all-from-out modname))
      (module+ safe
        (require (submod modname safe))
        (provide (all-from-out (submod modname safe))))) ...))


(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))

;; convert calling pattern to form (id contract body-exp)
;; hoist contract out of lambda-exp entirely
(define-syntax-rule (lambdafy-with-contract stx)
  (syntax-case stx ()
    
    [(_ id-exp contract lambda-exp) ; matches exactly three args after `define`
     ;; `normalize-definition` can't handle the acceptable `define/contract` pattern of id, contract, lambda exp after the `define`.
     ;; so extract the contract, and then put id & lambda-exp back together, and let `normalize-definition` destructure as usual.
     (with-syntax ([(new-id new-lambda-exp) (values->list (normalize-definition #'(_ id-exp lambda-exp) (datum->syntax #'id-exp 'λ) #t #t))])
       #'(new-id contract new-lambda-exp))]
    
    [(_ id-exp maybe-contract body-exp (... ...)) ; matches two, or four or more
     (with-syntax ([(id (lambda args contract body-exp (... ...))) (values->list (normalize-definition stx (datum->syntax #'id-exp 'λ) #t #t))])
       ;; lambda-exp = #'(lambda args body-exp (... ...))
       #'(id contract (lambda args body-exp (... ...))))]
    
    [else ; matches zero or one arugments 
     (error 'define-macro "not enough arguments")]))


;; convert calling pattern to form (id body-exp)
(define-syntax-rule (lambdafy stx)
  (with-syntax ([(id lambda-exp) (let-values ([(id-stx body-exp-stx) (normalize-definition stx (datum->syntax stx 'λ) #t #t)])
                                   (list id-stx body-exp-stx))])
    #'(id lambda-exp)))