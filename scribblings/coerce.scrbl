#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{Coercion}
@defmodule[sugar/coerce]

Functions that coerce the datatype of a value to another type. Racket already has type-specific conversion functions. But if you're handling values of indeterminate type — as sometimes happens in an untyped language — then handling the possible cases individually gets to be a drag.

@section{Values}

@defproc[
(->int
[v any/c])
integer?]
Convert @racket[_v] to an integer in the least surprising way, or raise an error if no conversion is possible.

Numbers are rounded down to the nearest integer. 

@examples[#:eval my-eval
(->int 3)
(->int 3.5)
(->int -2.5)
(->int (+ 3 (/ 1 2)))]

Stringlike values — paths, symbols, and strings — are converted to numbers and rounded down. 

@examples[#:eval my-eval
(->int "3.5")
(->int '3.5)
(->int (string->path "3.5"))]

Characters are directly converted to integers. 

@examples[#:eval my-eval
(->int #\A)
(->int #\◊)]

Lists, vectors, and other multi-value datatypes return their length (using @racket[len]).

@examples[#:eval my-eval
(->int (list 5 6 7))
(->int (hash 'a 1 'b 2 'c 3))]

The function will raise an error if no sensible conversion is possible.
@examples[#:eval my-eval
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
Same as @racket[->string], but return a symbol rather than a string.

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
Same as @racket[->string], but return a path (or complete path) rather than a string.

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
If @racket[_v] is a listlike data type — a vector, set, stream, sequence, or list — convert it to a list. A hash or dictionary becomes a list using @racket[dict->list]. If @racket[_v] is an atomic value, turn it into a single-member list. 

Note that a string is treated as an atomic value rather than decomposed with @racket[string->list]. This is done so the function handles strings the same way as symbols and paths.

@examples[#:eval my-eval
(->list '(a b c))
(->list (list->vector '(a b c)))
(->list (make-hash '((k . v) (k2 . v2))))
(->list "string") 
(->list 'symbol)
(->list (string->path "path"))
(->list +)
]

@defproc[
(->vector
[v any/c])
vector?]
Same as @racket[->list], but returns a vector rather than a list.

@examples[#:eval my-eval
(->vector '(a b c))
(->vector (list->vector '(a b c)))
(->vector (make-hash '((k . v) (k2 . v2))))
(->vector "string") 
(->vector 'symbol)
(->vector (string->path "path"))
(->vector +)
]

@defproc[
(->boolean
[v any/c])
boolean?]
Return @racket[#t] for all @racket[_v] except @racket[#f], which remains @racket[#f].

@examples[#:eval my-eval
(->boolean "string")
(->boolean 'symbol)
(->boolean +)
(->boolean '(l i s t))
(->boolean #f)
]


@deftogether[(
@defproc[(intish? [v any/c]) boolean?]
@defproc[(stringish? [v any/c]) boolean?]
@defproc[(symbolish? [v any/c]) boolean?]
@defproc[(pathish? [v any/c]) boolean?]
@defproc[(complete-pathish? [v any/c]) boolean?]
@defproc[(listish? [v any/c]) boolean?]
@defproc[(vectorish? [v any/c]) boolean?]
)]
Predicates that report whether @racket[_v] can be coerced to the specified type.

@examples[#:eval my-eval
(map intish? (list 3 3.5 #\A "A" + #t)) 
(map stringish? (list 3 3.5 #\A "A" + #t)) 
(map symbolish? (list 3 3.5 #\A "A" + #t)) 
(map pathish? (list 3 3.5 #\A "A" + #t)) 
(map complete-pathish? (list 3 3.5 #\A "A" + #t)) 
(map listish? (list 3 3.5 #\A "A" + #t)) 
(map vectorish? (list 3 3.5 #\A "A" + #t)) 
]



@section{Coercion contracts}

@deftogether[(
@defproc[(coerce/int? [v any/c]) integer?]
@defproc[(coerce/string? [v any/c]) string?]
@defproc[(coerce/symbol? [v any/c]) symbol?]
@defproc[(coerce/path? [v any/c]) path?]
@defproc[(coerce/boolean? [v any/c]) boolean?]
@defproc[(coerce/list? [v any/c]) list?]
)]
If @racket[_v] can be coerced to the specified type, change it to that type, then return it. If not, raise the usual contract error. These contracts can be used with input or output values. 

@examples[#:eval my-eval
(define/contract (add-ints x y)
    (coerce/int? coerce/int? . -> . any/c)
    (+ x y)) 
(code:comment @#,t{Input arguments will be coerced to integers, then added})
(add-ints 1.6 3.8)
(define/contract (int-sum x y)
    (any/c any/c . -> . coerce/int?)
    (+ x y))
(code:comment @#,t{Input arguments will be added, and the result coerced to an integer})
    (int-sum 1.6 3.8)
]


Please note: this is not an officially sanctioned way to use Racket's contract system, because contracts aren't supposed to mutate their values (see @racket[make-contract]).

But coercion contracts can be useful in two situations:

@itemlist[

@item{You want to be liberal about input types, but don't want to deal with the housekeeping and manual conversions between types.}

@item{Your contract involves an expensive operation that you'd rather avoid performing twice.}


]



