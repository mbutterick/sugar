#lang scribble/manual

@(require scribble/eval (for-label racket "../main.rkt" sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Coercion}
@defmodule[sugar/coerce]

@section{Values}
@defmodule[sugar/coerce/value]

@defproc[
(->int
[v any/c])
integer?]
Convert @racket[_v] to an integer in the least surprising way possible, or raise an error if it can't be done

Numbers are rounded down to the nearest integer. 

Stringlike values are converted to numbers and rounded down. 

Characters are directly converted to integers. 

Lists, vectors, and other multi-value datatypes return their length (using @racket[len]).

@examples[#:eval my-eval
(map ->int (list 3 3.5 -2.5 (+ 3 (/ 1 2))))
(map ->int (list "3.5" '3.5 (string->path "3.5")))
(->int (list 5 6 7))
(->int (hash 'a 1 'b 2 'c 3))
(->int #t)
]

@defproc[
(->string
[v any/c])
string?]
Return the most natural string representation of @racket[_v], or raise an error if none exists.

@examples[#:eval my-eval
(->string "string") 
(->string 'symbol)
(->string 98.6)
(->string (string->path "stdio.h"))
(->string #\u0041)
(->string #t)
]

@defproc[
(->symbol
[v any/c])
symbol?]
Same as @racket[->string], but returns a symbol rather than a string.

@examples[#:eval my-eval
(->symbol "string") 
(->symbol 'symbol)
(->symbol 98.6)
(->symbol (string->path "stdio.h"))
(->symbol #\u0041)
(->symbol #t)
]

@deftogether[(
@defproc[
(->path
[v any/c])
path?]

@defproc[
(->complete-path
[v any/c])
complete-path?]
)]
Same as @racket[->string], but returns a path (or complete path) rather than a string.

@examples[#:eval my-eval
(->path "string") 
(->path 'symbol)
(->complete-path 98.6)
(->complete-path (string->path "stdio.h"))
(->complete-path #\u0041)
(->complete-path #t)
]


@defproc[
(->list
[v any/c])
list?]
Convert a listlike @racket[_v] into a list, or put an atomic @racket[_v] into a single-member list. 

@examples[#:eval my-eval
(->list "string") 
(->list 'symbol)
(->list +)
(->list '(a b c))
(->list (list->vector '(a b c)))
]

@defproc[
(->vector
[v any/c])
vector?]
Same as @racket[->list], but returns a vector rather than a list.

@examples[#:eval my-eval
(->vector "string") 
(->vector 'symbol)
(->vector +)
(->vector '(a b c))
(->vector (list->vector '(a b c)))
]

@defproc[
(->boolean
[v any/c])
boolean?]
Return @racket[#t] for any @racket[_v] except @racket[#f], which remains @racket[#f]. Same as @code{(and v #t)}. 

@examples[#:eval my-eval
(map ->boolean (list "string" 'symbol + '(l i s t) #f))
]


@deftogether[(
@defproc[(intish? [v any/c]) boolean?]
@defproc[(stringish? [v any/c]) boolean?]
@defproc[(symbolish? [v any/c]) boolean?]
@defproc[(pathish? [v any/c]) boolean?]
@defproc[(complete-pathish? [v any/c]) boolean?]
)]
Report whether @racket[_v] can be coerced to the specified type.

@examples[#:eval my-eval
(map intish? (list 3 3.5 #\A "A" + #t)) 
(map stringish? (list 3 3.5 #\A "A" + #t)) 
(map symbolish? (list 3 3.5 #\A "A" + #t)) 
(map pathish? (list 3 3.5 #\A "A" + #t)) 
(map complete-pathish? (list 3 3.5 #\A "A" + #t)) 

]



@section{Contracts that coerce}
@defmodule[sugar/coerce/contract]

@deftogether[(
@defproc[(coerce/int? [v any/c]) int?]
@defproc[(coerce/string? [v any/c]) string]
@defproc[(coerce/symbol? [v any/c]) symbol]
@defproc[(coerce/path? [v any/c]) path?]
@defproc[(coerce/boolean? [v any/c]) boolean?]
)]
If @racket[_v] can be coerced to the specified type, these contracts will return it so coerced. If not, they raise the usual contract error. This is an unusual way to use contracts, but it can be handy.

@examples[#:eval my-eval
(define/contract (add-ints x y)
    (coerce/int? coerce/int? . -> . any/c)
    (+ x y)) 
(add-ints 1.6 3.8)
(define/contract (int-sum x y)
    (any/c any/c . -> . coerce/int?)
    (+ x y))
    (int-sum 1.6 3.8)
]



