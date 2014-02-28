#lang racket/base
(require (for-syntax racket/base))

(provide macro-map)

(define-syntax (macro-map stx)
  (syntax-case stx ()
    [(_ macro-name item)
     #'(cons (macro-name item) '())]
    [(_ macro-name item0 items ...)
     #'(cons
         (macro-name item0)
         (macro-map macro-name items ...))]))
