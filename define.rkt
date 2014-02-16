#lang racket/base
(require (for-syntax racket/base))
(require racket/contract)

(provide define/provide define+provide/contract define/contract+provide)

(define-syntax (define/provide stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) body ...)
     #'(define/provide proc
         (λ(arg ... . rest-arg) body ...))]
    [(_ name body ...)
     #'(begin
         (provide name)
         (define name body ...))]))

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
