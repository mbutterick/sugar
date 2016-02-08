#lang racket/base

(define-syntax-rule (r+p modname)
  (begin
    (require modname)
    (provide (all-from-out modname))
    (module+ safe
      (require (submod modname safe))
      (provide (all-from-out (submod modname safe))))))

(r+p "cache.rkt")
(r+p "coerce.rkt")
(r+p "debug.rkt")
(r+p "define.rkt")
(r+p "file.rkt")
(r+p "list.rkt")
(r+p "test.rkt")
