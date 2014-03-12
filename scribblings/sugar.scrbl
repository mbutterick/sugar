#lang scribble/manual

@(require scribble/eval (for-label racket "../main.rkt" sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))


@title{Sugar}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

A collection of tiny functions to help make Racket code more readable.

@section{Installation & updates}

At the command line:
@verbatim{raco pkg install sugar}

After that, you can update the package from the command line:
@verbatim{raco pkg update sugar}


@section{Coercion}
@defmodule[sugar/coerce]

@subsection{Values}
@defmodule[sugar/coerce/value]

@defproc[
(->int
[v any/c])
integer?]
Convert @racket[_v] to an integer in the least surprising way possible, or raise an error if none exists.

Numbers are rounded down to the nearest integer. 

Stringlike values are converted to numbers and rounded down. 

Characters are directly converted to integers. 

Lists, vectors, and other multi-value datatypes go through @racket[len].

@examples[#:eval my-eval
(map ->int (list 3 3.5 -2.5 (+ 3 (/ 1 2))))
(map ->int (list "3.5" '3.5 (string->path "3.5")))
(->int (list 5 6 7))
(->int (hash 'a 1 'b 2 'c 3))
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
(map ->boolean (list "string" 'symbol + '(a) #f))
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



@subsection{Contracts}
@defmodule[sugar/coerce/contract]


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/sugar"]{http://github.com/mbutterick/sugar}. Suggestions & corrections welcome.

