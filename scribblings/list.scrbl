#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar racket/list))

@title{List}
@defmodule[sugar/list]


@defproc[
(trimf
[lst list?]
[pred procedure?])
list?]
Drop elements from each end of @racket[_lst] that satisfy @racket[_pred]. Exactly equivalent to @racket[(dropf-right (dropf _lst _pred) _pred)].

@examples[#:eval my-eval
(trimf '(1 2 3 a b c 4 5 6) integer?)
(trimf '(1 2 3 a b c) integer?)
(trimf '(a b c) integer?)
(trimf '(a b c 1 2 3 d e f) integer?)]


@defproc[
(filter-split
[lst list?]
[pred procedure?])
(listof list?)]
Like @racket[string-split], but for lists. Drop elements from anywhere in @racket[_lst] that satisfy @racket[_pred] — ends, middle, you name it — and return a list of the sublists that remain.

@examples[#:eval my-eval
(filter-split '(1 a b c 2 d e f 3) integer?)
(filter-split '(1 a b c 2 d e f 3) (compose not integer?))
(filter-split '(a b c 1 2 3 d e f) integer?)
(filter-split '(a b c 1 2 3 d e f) (compose not integer?))]


@defproc[
(frequency-hash
[lst list?])
hash?]
Count the frequency of each element in @racket[_lst], and return a hash whose keys are the unique elements of @racket[_lst], and each value is the frequency of that element within @racket[_lst].

@examples[#:eval my-eval
(frequency-hash '(a b b c c c))
(frequency-hash '(c b c a b c))
]



@defproc[
(members-unique?
[container (or/c list? vector? string?)])
boolean?]
Return @racket[#t] if every element in @racket[_container] is unique, otherwise @racket[#f].

@examples[#:eval my-eval
(members-unique? '(a b c d e f))
(members-unique? '(a b c d e f a))
]

@defproc[
(members-unique?/error
[container (or/c list? vector? string?)])
boolean?]
Same as @racket[members-unique?], but if the members are not unique, raises a descriptive error rather than returning @racket[#f].

@examples[#:eval my-eval
(members-unique?/error '(a b c d e f))
(members-unique?/error '(a b c d e f a))
(members-unique?/error '(a b c d e f a b))
]

@defform[(when/splice test expr)]
A special version of @racket[when] that you can use inside @racket[quasiquote] to suppress @racket[void] values when @racket[_test] is @racket[#f]. As the name suggests, it works in conjunction with the @litchar["@"] splicing operator.

@examples[#:eval my-eval
`(,(when (even? 2) "hooray"))
`(,(when (even? 3) "hooray"))
`(,@(when/splice (even? 2) "hooray"))
`(,@(when/splice (even? 3) "hooray"))
]

@defform[(values->list values)]
Convert @racket[_values] to a simple list.

@examples[#:eval my-eval
(split-at '(a b c d e f) 3)
(values->list (split-at '(a b c d e f) 3))
]
