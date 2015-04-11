#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Cache}
@defmodule[#:multi (sugar/cache (submod sugar/cache safe) typed/sugar/cache)]

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

Keep in mind that the cache is only available to external callers of the resulting function. So if @racket[_proc] calls itself recursively, these calls are @italic{not} accelerated by the cache. If that's the behavior you need, use @racket[define/caching] to create a new recursive function.

@defform[(define/caching (name arg ... . rest-arg) body ...)]
Like @racket[define], but automatically uses @racket[make-caching-proc] to define a caching version of the function. If the function is recursive, the cache will be used for the recursive calls.

In the example below, @racketfont{fib} is a recursive function. Notice that simply wrapping the function in @racket[make-caching-proc] doesn't work in this case, because @racketfont{fib}'s recursive calls to itself bypass the cache. But @racketfont{fib-fast} is rewritten to recur on the caching function, and the caching works as expected.

@examples[#:eval my-eval
(define (fib x) 
  (if (< x 2) 1 (+ (fib (- x 1)) (fib (- x 2)))))
(define fibber (make-caching-proc fib))
(define/caching (fib-fast x) 
  (if (< x 2) 1 (+ (fib-fast (- x 1)) (fib-fast (- x 2)))))
(time (fib 32))
(time (fibber 32))
(time (fib-fast 32))
]