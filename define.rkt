#lang racket/base
(require (for-syntax racket/base))
(require racket/contract)


(provide define/provide define/provide/contract)

;; todo: these won't handle nested forms like (define ((foo x) y))

(define-syntax (define/provide stx)
  (syntax-case stx ()
    ;; order of cases matters, of course
    ;; match more complicated shape first, 
    ;; otherwise second matcher gives false positives
    [(_ (name arg ... . rest-arg) body ...)
     #'(begin
         (provide name)
         (define (name arg ... . rest-arg) body ...))]
    [(_ (name arg ...) body ...)
     #'(begin
         (provide name)
         (define (name arg ...) body ...))]   
    [(_ name body ...)
     #'(begin
         (provide name)
         (define name body ...))]))


(define-syntax (define/provide/contract stx)
  (syntax-case stx ()
    [(_ (name arg ... . rest-arg) contract body ...)
     #'(begin
         (provide (contract-out [name contract]))
         (define (name arg ... . rest-arg) body ...))]
    [(_ (name arg ...) contract body ...)
     #'(begin
         (provide (contract-out [name contract]))
         (define (name arg ...) body ...))]
    [(_ name contract body ...)
     #'(begin
         (provide (contract-out [name contract]))
         (define name body ...))]))

(define/provide (foo x y . z)
  (+ x y z))