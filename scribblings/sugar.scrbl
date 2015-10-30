#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))


@title[#:style 'toc]{Sugar}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodule[#:multi (sugar (submod sugar safe))]

A collection of small functions to help make Racket code simpler & more readable.

Sugar can be invoked two ways: as an ordinary library, or as a library with contracts (using the @tt{safe} submodule).


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
