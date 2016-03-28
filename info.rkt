#lang info
(define collection 'multi)
(define version "0.2")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("sugar/scribblings/sugar.scrbl" ())))
(define compile-omit-paths '("sugar/test"))