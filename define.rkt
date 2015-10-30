#lang racket/base
(require (for-syntax racket/base))
(require racket/contract)

(provide (all-from-out racket/contract))

;; get gets of typed source file, recompile it without typing in a submodule,
;; then require those identifiers into the current level.
(define-syntax (require-via-wormhole stx)
  (syntax-case stx ()
    [(_ path-spec)
     (let ([mod-name (gensym)])
       ;; need to use stx as context to get correct require behavior
       (datum->syntax stx `(begin
                             (module mod-name typed/racket/base/no-check
                               (require sugar/include)
                               (include-without-lang-line ,(syntax->datum #'path-spec)))
                             (require (quote mod-name)))))]))

;; each define macro recursively converts any form of define
;; into its lambda form (define name body ...) and then operates on that.

(define-syntax (make-safe-module stx)
  (syntax-case stx ()
    [(_ name contract)
     #'(module+ safe 
         (require racket/contract)
         (provide (contract-out [name contract])))]
    [(_ name)
     #'(module+ safe 
         (provide name))]))

(define-syntax (define+provide+safe stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define+provide+safe proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (define name body ...)
         (provide name)
         (make-safe-module name contract))]))

;; for previously defined identifiers
;; takes args like (provide+safe [ident contract]) or just (provide+safe ident)
;; any number of args.
(define-syntax (provide+safe stx)
  (syntax-case stx () 
    [(_ items ...)
     (datum->syntax stx
                    `(begin
                       ,@(for/list ([item (in-list (syntax->datum #'(items ...)))])
                           (define-values (name contract) (if (pair? item)
                                                              (values (car item) (cadr item))
                                                              (values item #f)))
                           `(begin
                              (provide ,name)
                              (make-safe-module ,name ,@(if contract (list contract) null))))))]))

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

(provide+safe require-via-wormhole
              make-safe-module
              define+provide+safe
              provide+safe
              define+provide/contract
              define/contract+provide
              define+provide)
