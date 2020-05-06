#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@title{Include}
@defmodule[sugar/include]

@defform[(include-without-lang-line path-spec)]
Inline the syntax in the file designated by @racket[_path-spec], after stripping off the @tt{#lang} line of the file (if it exists, otherwise just @racket[include] the file as usual). 