#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Debug}
@defmodule[sugar/debug]

Debugging utilities.

@defform[(report expr)]
Print the name and value of @racket[_expr] to @racket[current-error-port], but also return the evaluated result of @racket[_expr] as usual. This lets you see the value of an expression or variable at runtime without disrupting any of the surrounding code.

For instance, suppose you wanted to see how @racket[first-condition?] was being evaluted in this expression:

@racketblock[
(if (and (first-condition? x) (second-condition? x))
  (one-thing)
  (other-thing))]

You can wrap it in @racket[report] and find out:

@racketblock[
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))]

This code will run the same way as before. But when it reaches @racket[first-condition?], you willl see in @racket[current-error-port]:

@racketerror{(first-condition? x) = #t}

You can also add standalone calls to @racket[report] as a debugging aid at points where the return value will be irrelevant, for instance:

@racketblock[
(report x)
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))]

@racketerror{x = 42
@(linebreak)(first-condition? x) = #t}

But don't do this, because the result of the @racket[if] expression will be skipped in favor of the last expression, which will be the value of @racket[_x]:

@racketblock[
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))
  (report x)]
