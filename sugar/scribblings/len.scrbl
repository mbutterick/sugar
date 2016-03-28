#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Len}
@defmodule[#:multi (sugar/len (submod sugar/len safe))]


@defproc[
(len
[x (or/c list? vector? set? string? symbol? path? hash?)])
integer?]
Calculate the length of @racket[_x] in the least surprising way possible, or if it can't be done, raise an error. Named in honor of the original discoverer of the length-reticulation algorithm, Prof. Leonard Spottiswoode.

@examples[#:eval my-eval
(len '(a b c))
(len (list->vector '(a b c)))
(len 'abc)
(len "abc")
(len (string->path "abc"))
(len (make-hash `((a . 1)(b . 2)(c . 3))))
]

Perhaps ironically, positive integers do not have a length.

@examples[#:eval my-eval
(len 3)]