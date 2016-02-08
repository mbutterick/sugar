#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Container}
@defmodule[#:multi (sugar/container (submod sugar/container safe))]

Type-neutral functions for getting elements out of a container, or testing membership.


@defproc[
(get
[container (or/c list? vector? sequence? dict? string? symbol? path?)]
[which any/c]
[end_which (or/c (and/c integer? positive?) #f) #f])
any/c]
For a @racket[_container] that's a @racket[dict?], retrieve the element associated with the key @racket[_which]. Raise an error if the key doesn't exist.

@examples[#:eval my-eval
(get (make-hash '((a . 1) (b . 2) (c  . 3))) 'b)
(get (make-hash '((a . 1) (b . 2) (c  . 3))) 'z)
]

For other @racket[_container] types — which are all sequence-like — retrieve the element located at @racket[_which]. Or if the optional @racket[_end_which] argument is provided, retrieve the elements from @racket[_which] to @racket[(sub1 _end_which)], inclusive (i.e., make a slice). Raise an error if @racket[_which] or @racket[_end_which] is out of bounds.

@examples[#:eval my-eval
(get '(0 1 2 3 4 5) 2)
(get '(0 1 2 3 4 5) 2 4)
(get '(0 1 2 3 4 5) 100)
(get '(0 1 2 3 4 5) 2 100)
(get (list->vector '(0 1 2 3 4 5)) 2)
(get (list->vector '(0 1 2 3 4 5)) 2 4)
(get "purple" 2)
(get "purple" 2 4)
(get 'purple 2)
(get 'purple 2 4)
]

When @racket[_container] is a path, it's treated as a list of path elements (created by @racket[explode-path]), not as a stringlike value.

@examples[#:eval my-eval
(get (string->path "/root/foo/bar/file.txt") 1)
(get (string->path "/root/foo/bar/file.txt") 0 3)
]

To slice to the end of @racket[_container], use @racket[(len _container)] as the value of @racket[_end_which].

@examples[#:eval my-eval
(define xs '(0 1 2 3 4 5))
(get xs 2 (len xs))
(get (list->vector xs) 2 (len (list->vector xs)))
(define color "purple")
(get color 2 (len color))
]


@defproc[
(in?
[item any/c]
[container (or/c list? vector? sequence? set? dict? string? symbol? path?)])
boolean?]
Return @racket[#t] if @racket[_item] is in @racket[_container], or @racket[#f] otherwise.

@examples[#:eval my-eval
(in? 2 '(0 1 2 3 4 5))
(in? 'a '(0 1 2 3 4 5))
(in? 2 (list->vector '(0 1 2 3 4 5)))
(in? "pu" "purple")
(in? "zig" "purple")
(in? 'b (make-hash '((a . 1) (b . 2) (c  . 3))))
(in? 'z (make-hash '((a . 1) (b . 2) (c  . 3))))
]

As with @racket[get], when @racket[_container] is a path, it's treated as a list of exploded path elements, not as a stringlike value.

@examples[#:eval my-eval
(in? "foo" (string->path "/root/foo/bar/file.txt"))
(in? "zam" (string->path "/root/foo/bar/file.txt"))
]

