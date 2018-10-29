#lang racket/base
(require (for-syntax racket/base)
         syntax/define)
(provide (except-out (all-defined-out) values->list))

(define-syntax-rule (require+provide/safe MODNAME ...)
  (begin
    (begin
      (require MODNAME)
      (provide (all-from-out MODNAME))
      (module+ safe
        (require (submod MODNAME safe))
        (provide (all-from-out (submod MODNAME safe))))) ...))

(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))

;; convert calling pattern to form (id contract body-exp)
;; hoist contract out of lambda-exp entirely
(define (lambdafy-with-contract stx)
  (syntax-case stx ()
    [(_ ID-EXP CONTRACT LAMBDA-EXP) ; matches exactly three args after `define`
     ;; `normalize-definition` can't handle the acceptable `define/contract` pattern of id, contract, lambda exp after the `define`.
     ;; so extract the contract, and then put id & lambda-exp back together, and let `normalize-definition` destructure as usual.
     (with-syntax ([(NEW-ID NEW-LAMBDA-EXP)
                    (values->list (normalize-definition #'(_ ID-EXP LAMBDA-EXP) (datum->syntax stx 'λ) #t #t))])
       #'(NEW-ID CONTRACT NEW-LAMBDA-EXP))]
    ;; matches two or more args (three-arg case handled above)
    [(_ ID-EXP . BODY)
     (with-syntax ([(NEW-ID (LAMBDA ARGS CONTRACT . NEW-BODY))
                    (values->list (normalize-definition stx (datum->syntax stx 'λ) #t #t))])
       ;; because the macro provides the `lambda` below, it takes the local srcloc by default
       ;; so `syntax/loc` applies the original srcloc (associated with args and body-exp)
       #`(NEW-ID CONTRACT #,(syntax/loc #'ID-EXP (LAMBDA ARGS . NEW-BODY))))]
    ;; matches zero or one arguments 
    [_ (raise-syntax-error 'define-macro "not enough arguments")]))

(define (lambdafy stx)
  (with-syntax ([(ID LAMBDA-EXP)
                 (values->list (normalize-definition stx (datum->syntax stx 'λ) #true #true))])
    #'(ID LAMBDA-EXP)))