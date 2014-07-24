#lang racket/base
(require (for-syntax racket/base))
(require racket/contract)

(provide (all-defined-out) (all-from-out racket/contract))

;; each define macro recursively converts any form of define
;; into its lambda form (define name body ...) and then operates on that.

(define-syntax (define+provide+safe stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define+provide+safe proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (define name body ...)
         (provide name)
         (module+ safe 
           (provide (contract-out [name contract]))))]))

(define-syntax (define+provide/contract stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define+provide/contract proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (provide (contract-out [name contract]))
         (define name body ...))]))


(define-syntax (define/contract+provide stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define/contract+provide proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (provide name)
         (define/contract name contract body ...))]))


(define-syntax (define+provide stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) body ...)
     #'(define+provide proc
         (λ(arg ... . rest-arg) body ...))]
    [(_ name body ...)
     #'(begin
         (provide name)
         (define name body ...))]))
