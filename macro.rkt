#lang racket/base
(require (for-syntax racket/base))

(provide macro-map)

(define-syntax (macro-map stx)
  (syntax-case stx ()
    [(_ macro-name (list item0))
     #'(cons
        (macro-name item0) '())]
    
    [(_ macro-name (list item0 items ...))
     #'(cons
        (macro-name item0)
        (macro-map macro-name (list items ...)))]))


(define-syntax-rule (add x)
  (+ 1 x))

(macro-map add (list 24 25 30))