#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@title{Include}
@defmodule[sugar/include]

@defform[(include-without-lang-line path)]
Like @racket[include], but strips off the @tt{#lang} line of the file. Why? So you can take the code from a working source file and recompile it under a different @tt{#lang}. Why? Well, you could take code from a @tt{#lang typed/racket} source file and recompile as @tt{#lang typed/racket/no-check}. Why? Because then you could invoke your code natively from typed and untyped environments. 

Please, don't use this on a file without a @tt{#lang} line. For that, just use @racket[include].