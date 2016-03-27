#lang info
(define collection "sugar")
(define version "0.2")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/sugar.scrbl" ())))
(define compile-omit-paths '("test"))