#lang racket/base
(require (for-syntax racket/base racket/syntax syntax/strip-context) racket/contract)


(define-syntax (make-safe-module stx)
  (syntax-case stx ()
    [(_ [id contract])
     ;; need to put `racket/contract` inside calling location's context
     (with-syntax ([require-racket-contract (datum->syntax #'id '(require racket/contract))])
       #'(module+ safe 
           require-racket-contract
           (provide (contract-out [id contract]))))]
    [(_ id)
     #'(module+ safe
         (provide id))]))


;; convert calling pattern to form (id contract body-exp)
(define-for-syntax (lambdafy-with-contract stx)
  (syntax-case stx ()
    [(_ (id arg ... . rest-arg) contract body ...)
     (replace-context #'id  #'(id contract (λ (arg ... . rest-arg) body ...)))]
    [(_ id contract lambda-exp)
     (replace-context #'id #'(id contract lambda-exp))]))


;; convert calling pattern to form (id body-exp)
(define-for-syntax (lambdafy stx)
  (syntax-case stx ()
    [(_ (id arg ... . rest-arg) body ...)
     (replace-context #'id #'(id (λ (arg ... . rest-arg) body ...)))]
    [(_ id lambda-exp)
     (replace-context #'id #'(id lambda-exp))]))


(define-syntax (define+provide+safe stx)
  (with-syntax ([(id contract lambda-exp) (lambdafy-with-contract stx)])
    #'(begin
        (define id lambda-exp)
        (provide id)
        (make-safe-module [id contract]))))


;; for previously defined identifiers
;; takes args like (provide+safe [id contract]) or just (provide+safe id)
;; any number of args.
(define-syntax-rule (provide+safe thing ...)
  (begin
    (provide+safe/once thing) ...))


;; `provide+safe` might have interleaved ids or [id contract] args so handle them individually.
(define-syntax (provide+safe/once stx)
  (with-syntax ([(id msm-arg) (syntax-case stx ()
                                [(_ [id contract])
                                 #'(id [id contract])]
                                [(_ id)
                                 #'(id id)])])
    #'(begin
        (provide id)
        (make-safe-module msm-arg))))


(define-syntax (define+provide/contract stx)
  (with-syntax* ([(id contract lambda-exp) (lambdafy-with-contract stx)]
                 [require-racket-contract (datum->syntax #'id '(require racket/contract))])
    #'(begin
        require-racket-contract
        (provide (contract-out [id contract]))
        (define id lambda-exp))))


(define-syntax (define/contract+provide stx)
  (with-syntax* ([(id contract lambda-exp) (lambdafy-with-contract stx)]
                 [require-racket-contract (datum->syntax #'id '(require racket/contract))])
    #'(begin
        require-racket-contract
        (provide id)
        (define/contract id contract lambda-exp))))


(define-syntax (define+provide stx)
  (with-syntax ([(id lambda-exp) (lambdafy stx)])
    #'(begin
        (provide id)
        (define id lambda-exp))))


(provide+safe make-safe-module
              define+provide+safe
              provide+safe
              define+provide/contract
              define/contract+provide
              define+provide)
