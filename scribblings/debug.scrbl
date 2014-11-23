#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Debug}
@defmodule[sugar/debug]

Debugging utilities.

@defform*[((report expr) (report expr maybe-name))]
Print the name and value of @racket[_expr] to @racket[current-error-port], but also return the evaluated result of @racket[_expr] as usual. This lets you see the value of an expression or variable at runtime without disrupting any of the surrounding code. Optionally, you can use @racket[_maybe-name] to change the name shown in @racket[current-error-port].

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
(report x x-before-function)
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))]

@racketerror{x-before-function = 42
@(linebreak)(first-condition? x) = #t}

But be careful — in the example below, the result of the @racket[if] expression will be skipped in favor of the last expression, which will be the value of @racket[_x]:

@racketblock[
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))
  (report x)]


@defform[(report* expr ...)]
Applies @racket[report] separately to each @racket[_expr] in the list.

@defform[(repeat num expr ...)]
Evaluates @racket[_expr] repeatedly — @racket[_num] times, in fact — and returns the last value.

@examples[#:eval my-eval
(repeat 1000 
(for/sum ([i (in-range 1000)]) i))
]


@defform[(time-repeat num expr ...)]
Shorthand for using @racket[time] with @racket[repeat]. Repeats the whole list of expressions, prints the total time, and returns the last value.

@examples[#:eval my-eval
(time-repeat 1000 
(for/product ([i (in-range 1000)]) i)
(for/sum ([i (in-range 1000)]) i))
]

@defform[(time-repeat* num expr ...)]
Applies @racket[time-repeat] to each @racket[_expr] individually.

@examples[#:eval my-eval
(time-repeat* 1000 
(for/product ([i (in-range 1000)]) i)
(for/sum ([i (in-range 1000)]) i))
]