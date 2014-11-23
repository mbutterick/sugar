#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Cache}
@defmodule[sugar/cache]

If, like Ricky Bobby and me, you want to go fast, then try using more caches. They're wicked fast.

@defproc[
(make-caching-proc
[proc procedure?])
procedure?]
Make a caching version of @racket[_proc]. This means a hash table will be attached to @racket[_proc], and result values will automatically be saved & retrieved. The arguments to the procedure are used as the hash key.

In the example below, notice that both invocations of @racketfont{slow-op} take approximately the same time, whereas the second invocation of @racketfont{fast-op} gets its value from the cache, and is thus nearly instantaneous.

@examples[#:eval my-eval
(define (slow-op x) (for/sum ([i (in-range 100000000)]) i))
(time (slow-op 42))
(time (slow-op 42))
(define fast-op (make-caching-proc slow-op))
(time (fast-op 42))
(time (fast-op 42))
]

@defform[(define/caching (name arg ... . rest-arg) body ...)]
Like @racket[define], but automatically uses @racket[make-caching-proc] to define a caching version of the function.
