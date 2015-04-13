#lang scribble/manual

@(require scribble/eval (for-label racket sugar (only-in typed/racket require/typed)))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))


@title[#:style 'toc]{Sugar}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodule[#:multi (sugar (submod sugar safe) typed/sugar)]

A collection of small functions to help make Racket code simpler & more readable.

Sugar can be invoked three ways: as an untyped library, as an untyped library with contracts (using the @tt{safe} submodule), or as a typed library. 

A few functions are only available as untyped or typed. These exceptions are noted below.

The typed version of Sugar is implemented ``natively'' in the sense that it is compiled separately with type annotations. It is not a @racket[require/typed] wrapper around the untyped code. This avoids the contract barrier that is otherwise automatically imposed between typed and untyped code. 

@margin-note{This cross-compiling technique relies on @racket[include-without-lang-line] in this library.}

@;local-table-of-contents[]

@include-section["installation.scrbl"]

@include-section["cache.scrbl"]

@include-section["coerce.scrbl"]

@include-section["container.scrbl"]

@include-section["debug.scrbl"]

@include-section["file.scrbl"]

@include-section["include.scrbl"]

@include-section["len.scrbl"]

@include-section["list.scrbl"]

@include-section["string.scrbl"]

@include-section["xml.scrbl"]

@include-section["license.scrbl"]

@;index-section[]
