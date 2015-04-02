#lang info
(define collection "sugar")
(define deps '(("base" #:version "6.0")))
(define build-deps '("scribble-lib"))
(define scribblings '(("scribblings/sugar.scrbl" ())))
(define compile-omit-paths '("test"))
