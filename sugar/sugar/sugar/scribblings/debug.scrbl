#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Debug}
@defmodule[#:multi (sugar/debug (submod sugar/debug safe))]

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


@defform*[((report/line expr) (report/line expr maybe-name))]
Same as @racket[report], but also shows the line number of @racket[_expr].

@defform*[((report/file expr) (report/file expr maybe-name))]
Same as @racket[report], but also shows the line number and source-file name of @racket[_expr].

@deftogether[(
@defform[(report* expr ...)]
@defform[(report*/line expr ...)]
@defform[(report*/file expr ...)]
)]
Apply the relevant @racket[report] macro separately to each @racket[_expr] in the list.

@defform[(repeat num expr ...)]
Evaluate @racket[_expr] repeatedly — @racket[_num] times, in fact — and return the last value.

@examples[#:eval my-eval
(repeat 1000 
(for/sum ([i (in-range 1000)]) i))
]


@defform[(time-repeat num expr ...)]
Shorthand for using @racket[time] with @racket[repeat]. Repeat the whole list of expressions, print the total time, and return the last value.

@examples[#:eval my-eval
(time-repeat 1000 
(for/product ([i (in-range 1000)]) i)
(for/sum ([i (in-range 1000)]) i))
]

@defform[(time-repeat* num expr ...)]
Apply @racket[time-repeat] to each @racket[_expr] individually.

@examples[#:eval my-eval
(time-repeat* 1000 
(for/product ([i (in-range 1000)]) i)
(for/sum ([i (in-range 1000)]) i))
]

@defform[(compare expr id id-alt ...)]
Evaluate @racket[_expr] first using @racket[_id], and then again substituting @racket[_id-alt] in place of @racket[_id], and then again for every other @racket[_id-alt] in the list. This is useful for comparing the performance of multiple versions of a function.

@examples[#:eval my-eval
(define (fib x) 
  (if (< x 2) 1 (+ (fib (- x 1)) (fib (- x 2)))))
(define/caching (fib-fast x) 
  (if (< x 2) 1 (+ (fib-fast (- x 1)) (fib-fast (- x 2)))))
(compare (time (fib 34)) fib fib-fast)
]

