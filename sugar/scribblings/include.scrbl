#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@title{Include}
@defmodule[sugar/include]

@defform[(include-without-lang-line path-spec)]
Inline the syntax in the file designated by @racket[_path-spec], after stripping off the @tt{#lang} line of the file (if it exists, otherwise just @racket[include] the file as usual). 

Why? So you can take the code from a working source file and recompile it under a different @tt{#lang}. Why? Well, you could take code from a @tt{#lang typed/racket} source file and recompile as @tt{#lang typed/racket/no-check}. Why? Because then you could make typed and untyped modules from the same code without the mandatory contracts imposed by @racket[require/typed].