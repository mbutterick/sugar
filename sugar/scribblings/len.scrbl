#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Len}
@defmodule[sugar/len]

@defproc[
(len
[x (or/c list? string? symbol? vector? hash? integer? set?)])
integer?]
Convert @racket[_x] to a length in the least surprising way possible, or raise an error if it can't be done.