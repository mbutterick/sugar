#lang typed/racket/base

(define-syntax-rule (r/p name)
  (begin
    (require name)
    (provide (all-from-out name))))

(r/p "sugar/cache.rkt")
(r/p "sugar/coerce.rkt")
(r/p "sugar/debug.rkt")
(r/p "sugar/define.rkt")
(r/p "sugar/file.rkt")
(r/p "sugar/len.rkt")
(r/p "sugar/list.rkt")
(r/p "sugar/misc.rkt")
(r/p "sugar/string.rkt")
(r/p "sugar/test.rkt")