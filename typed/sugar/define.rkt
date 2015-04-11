#lang typed/racket/base/no-check
;; use of no-check is deliberate here.
;; these helper macros don't do any type checking, just rearranging
;; they can't be combined with the untyped define macros, however
;; because the -> symbol is defined differently here
(require (for-syntax typed/racket/base racket/syntax))
(provide (all-defined-out))

(define-syntax (define/typed stx)
  (syntax-case stx ()
    [(_ (proc-name arg ... . rest-arg) type-expr body ...)
     #'(define/typed proc-name type-expr
         (λ(arg ... . rest-arg) body ...))]
    [(_ proc-name type-expr body ...)
     #'(begin
         (: proc-name type-expr)
         (define proc-name body ...))]))

(define-syntax (define/typed+provide stx)
  (syntax-case stx ()
    [(_ (proc-name arg ... . rest-arg) type-expr body ...)
     #'(begin
         (provide proc-name)
         (define/typed proc-name type-expr
           (λ(arg ... . rest-arg) body ...)))]
    [(_ proc-name type-expr body ...)
     #'(begin
         (provide proc-name)
         (begin
           (: proc-name : type-expr)
           (define proc-name body ...)))]))

(define-syntax (define-type+predicate stx)
  (syntax-case stx ()
    [(_ id basetype)
     (with-syntax ([id? (format-id stx "~a?" #'id)])
       #'(begin
           (define-type id basetype)
           (define-predicate id? id)))]))