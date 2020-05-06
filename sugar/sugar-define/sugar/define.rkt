#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/strip-context
                     "private/syntax-utils.rkt")
         racket/contract)

(define-syntax (make-safe-module stx)
  (syntax-case stx ()
    [(_ [ID CONTRACT])
     ;; need to put `racket/contract` inside calling location's context
     (with-syntax ([RACKET/CONTRACT (datum->syntax #'ID 'racket/contract)])
       #'(module+ safe 
           (require RACKET/CONTRACT)
           (provide (contract-out [ID CONTRACT]))))]
    [(_ ID)
     #'(module+ safe
         (provide ID))]))

(define-syntax (define+provide+safe stx)
  (with-syntax ([(ID CONTRACT LAMBDA-EXP) (lambdafy-with-contract stx)])
    #'(begin
        (define ID LAMBDA-EXP)
        (provide+safe [ID CONTRACT]))))

;; for previously defined identifiers
;; takes args like (provide+safe [id contract]) or just (provide+safe id)
;; any number of args.
(define-syntax-rule (provide+safe THING ...)
  (begin
    (provide+safe/once THING) ...))

;; `provide+safe` might have interleaved ids or [id contract] args so handle them individually.
(define-syntax (provide+safe/once stx)
  (with-syntax ([(ID MSM-ARG) (syntax-case stx ()
                                [(_ [ID contract])
                                 #'(ID [ID contract])]
                                [(_ id)
                                 #'(id id)])])
    #'(begin
        (provide ID)
        (make-safe-module MSM-ARG))))

(define-syntax (define+provide/contract stx)
  (with-syntax* ([(ID CONTRACT LAMBDA-EXP) (lambdafy-with-contract stx)]
                 [RACKET/CONTRACT (datum->syntax #'ID 'racket/contract)])
    #'(begin
        (require RACKET/CONTRACT)
        (provide (contract-out [ID CONTRACT]))
        (define ID LAMBDA-EXP))))

(define-syntax (define/contract+provide stx)
  (with-syntax* ([(ID CONTRACT LAMBDA-EXP) (lambdafy-with-contract stx)]
                 [RACKET/CONTRACT (datum->syntax #'ID 'racket/contract)])
    #'(begin
        (require RACKET/CONTRACT)
        (provide ID)
        (define/contract ID CONTRACT LAMBDA-EXP))))

(define-syntax (define+provide stx)
  (with-syntax ([(ID LAMBDA-EXP) (lambdafy stx)])
    #'(begin
        (provide ID)
        (define ID LAMBDA-EXP))))

(provide+safe make-safe-module
              define+provide+safe
              provide+safe
              define+provide/contract
              define/contract+provide
              define+provide)
