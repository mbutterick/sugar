#lang racket/base

(define-syntax-rule (r+p modname ...)
  (begin
    (begin
      (require modname)
      (provide (all-from-out modname))
      (module+ safe
        (require (submod modname safe))
        (provide (all-from-out (submod modname safe))))) ...))

(r+p "cache.rkt"
     "coerce.rkt"
     "debug.rkt"
     "define.rkt"
     "file.rkt"
     "list.rkt"
     "test.rkt"
     "xml.rkt")