#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{String}
@defmodule[sugar/string]


@defproc[
(starts-with?
[str stringish?]
[starter stringish?])
boolean?]
Return @racket[#t] if @racket[_str] starts with @racket[_starter], otherwise @racket[#f].

@examples[#:eval my-eval
(starts-with? "foobar" "foo")
(starts-with? "foobar" "foobar")
(starts-with? "foobar" "zam")
(starts-with? "foobar" "foobars")
]


@defproc[
(ends-with?
[str stringish?]
[ender stringish?])
boolean?]
Return @racket[#t] if @racket[_str] ends with @racket[_ender], otherwise @racket[#f].

@examples[#:eval my-eval
(ends-with? "foobar" "foo")
(ends-with? "foobar" "foobar")
(ends-with? "foobar" "zam")
(ends-with? "foobar" "foobars")
]


@defproc[
(capitalized?
[str stringish?])
boolean?]
Return @racket[#t] if @racket[_str] starts with a capital letter, otherwise @racket[#f].

@examples[#:eval my-eval
(capitalized? "Brennan")
(capitalized? "Brennan stinks")
(capitalized? "stinks")
]

