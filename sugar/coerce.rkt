#lang racket/base

(define-syntax-rule (r+p modname ...)
  (begin
    (begin
      (require modname)
      (provide (all-from-out modname))
      (module+ safe
        (require (submod modname safe))
        (provide (all-from-out (submod modname safe))))) ...))

(r+p "coerce/base.rkt" "coerce/contract.rkt")