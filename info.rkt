#lang info
(define collection "sugar")
(define deps '("base" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/sugar.scrbl" ())))
(define compile-omit-paths '("test"))