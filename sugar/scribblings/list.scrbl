#lang scribble/manual

@(require scribble/eval (for-label racket sugar racket/function))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar racket/list racket/function))

@title{Lists}
@defmodule[#:multi (sugar/list (submod sugar/list safe))]



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
(filter-split '(1 a b c 2 d e f 3) (negate integer?))]

@defproc[
(partition*
[pred procedure?]
[lst list?])
(values list? list?)]
Like @racket[partition], but contiguous groups of elements matching (or not matching) @racket[_pred] are kept together in sublists.

Same as @racket[(values (filter-split _lst _pred) (filter-split _lst (negate _pred)))], but only traverses the list once.

@examples[#:eval my-eval
(partition* integer? '(1 a b c 2 d e f 3))
(partition* (negate integer?) '(1 a b c 2 d e f 3))]

@defproc[
(slice-at
[lst list?]
[len (and/c integer? positive?)]
[force? boolean? #f])
(listof list?)]
Divide @racket[_lst] into sublists of length @racket[_len]. If @racket[_lst] cannot be divided evenly by @racket[_len], the last sublist will be shorter. If this displeases you, set @racket[_force?] to @racket[#t] and a stumpy final sublist will be ignored.

@examples[#:eval my-eval
(slice-at (range 5) 1)
(slice-at (range 5) 2)
(slice-at (range 5) 2 #t)
(slice-at (range 5) 3)
(slice-at (range 5) 5)
(slice-at (range 5) 5 #t)
(slice-at (range 5) 100000)
(slice-at (range 5) 100000 #t)]

@defproc[
(slicef
[lst list?]
[pred procedure?])
(listof list?)]
Divide @racket[_lst] into sublists that are homogeneously @racket[_pred] or not @racket[_pred]. If none of the elements match @racket[_pred], there is no slice to be made, and the result is the whole input list.

@examples[#:eval my-eval
(slicef '(1 2 2 1 2) even?)
(slicef (range 5) odd?)
(slicef (range 5) string?)]


@defproc[
(slicef-at
[lst list?]
[pred procedure?]
[force? boolean? #f])
(listof list?)]
Divide @racket[_lst] into sublists, each starting with an element matching @racket[_pred]. The first element of the first sublist may not match @racket[_pred]. But if you really & truly want only the sublists starting with an element matching @racket[_pred], set @racket[_force?] to @racket[#true].

If none of the elements match @racket[_pred], there is no slice to be made, and the result is the whole input list.

@examples[#:eval my-eval
(slicef-at (range 5) even?)
(slicef-at '(1 2 2 1 2) even?)
(slicef-at '(1 2 2 1 2) even? #t)
(slicef-at (range 5) odd?)
(slicef-at (range 5) odd? #t)]

@defproc[
(slicef-after
[lst list?]
[pred procedure?]
[force? boolean? #f])
(listof list?)]
Divide @racket[_lst] into sublists, each ending with an element matching @racket[_pred]. The last element of the last sublist may not match @racket[_pred]. But if you really & truly want only the sublists ending with an element matching @racket[_pred], set @racket[_force?] to @racket[#true]. 

If none of the elements match @racket[_pred], there is no slice to be made, and the result is the whole input list.

@examples[#:eval my-eval
(slicef-after '(1 2 2 1 2) even?)
(slicef-after (range 5) odd?)
(slicef-after (range 5) odd? #true)
(slicef-after (range 5) string?)]


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


@defform[(values->list values)]
Convert @racket[_values] to a simple list.

@examples[#:eval my-eval
(split-at '(a b c d e f) 3)
(values->list (split-at '(a b c d e f) 3))
]


@defproc[
(sublist
[lst list?]
[start-idx (and/c integer? (not/c negative?))]
[end-idx (and/c integer? (not/c negative?))])
list?]
Return a sublist of the @racket[_lst] starting with item @racket[_start-idx] and ending one item @bold{before} item @racket[_end-idx]. (Similar to how list slices are denominated in Python.) Thus the maximum value for @racket[_end-idx] is @racketfont{(length @racket[_lst])}. Errors will be triggered by nonsensical values for @racket[_end-idx].

Bear in mind that @racket[sublist] is built for convenience, not performance. If you need to do a lot of random access into the middle of an ordered sequence of items, you'd be better off putting them into a @racket[vector] and using @racket[vector-copy].

@examples[#:eval my-eval
(sublist '(0 1 2 3 4 5 6 7 8) 0 8)
(sublist '(0 1 2 3 4 5 6 7 8) 8 9)
(sublist '(0 1 2 3 4 5 6 7 8) 2 5)
(sublist '(0 1 2 3 4 5 6 7 8) 5 2)
(sublist '(0 1 2 3 4 5 6 7 8) 2 10)
]


@defproc[
(break-at
[lst list?]
[indexes (or/c integer? (listof integer?))])
(listof list?)]
Break @racket[_lst] into smaller lists at the index positions in @racket[_indexes]. If a single integer value is given for @racket[_indexes], it's treated as a one-element list. Errors will arise if a breakpoint index exceeds the length of the list, or if the breakpoints are not increasing.

@examples[#:eval my-eval
(break-at '(0 1 2 3 4 5 6 7 8) 3)
(break-at '(0 1 2 3 4 5 6 7 8) '(3))
(break-at '(0 1 2 3 4 5 6 7 8) '(3 6))
(break-at '(0 1 2 3 4 5 6 7 8) '(3 6 8))
(break-at '(0 1 2 3 4 5 6 7 8) '(3 6 8 10))
]

@defproc[
(shift
[lst list?]
[how-far integer?]
[fill-item any/c #f]
[cycle? boolean? #f])
list?]
Move the items in @racket[_lst] to the right (if @racket[_how-far] is positive) or left (if @racket[_how-far] is negative). By default, vacated spaces in the list are filled with @racket[_fill-item]. But if @racket[_cycle?] is true, elements of the list wrap around (and @racket[_fill-item] is ignored). Either way, the result list is always the same length as the input list. (If you don't care about the lengths being the same, you probably want @racket[take] or @racket[drop] instead.) If @racket[_how-far] is 0, return the original list. If @racket[_how-far] is bigger than the length of @racket[_lst], and @racket[_cycle] is not true, raise an error.

@examples[#:eval my-eval
(define xs (range 5))
(shift xs 2)
(shift xs -2 0)
(shift xs 2 'boing)
(shift xs 2 'boing #t)
(shift xs 0)
(shift xs 42)
]

@defproc[
(shift-left
[lst list?]
[how-far integer?]
[fill-item any/c #f]
[cycle? boolean? #f])
list?]
Like @racket[shift], but the list is shifted left when @racket[_how-far] is positive, and right when it's negative. Otherwise identical.

@examples[#:eval my-eval
(define xs (range 5))
(shift-left xs 2)
(shift-left xs -2 0)
(shift-left xs 2 'boing)
(shift-left xs 2 'boing #t)
(shift-left xs 0)
(shift-left xs 42)
]

@deftogether[(
@defproc[
(shift-cycle
[lst list?]
[how-far integer?])
list?]
@defproc[
(shift-left-cycle
[lst list?]
[how-far integer?])
list?]
)]
Like @racket[shift] and @racket[shift-left], but automatically invokes cycle mode. @racket[_how-far] can be any size.

@examples[#:eval my-eval
(define xs (range 5))
(shift-cycle xs 2)
(shift-cycle xs -2)
(shift-cycle xs 0)
(shift-cycle xs 42)
(shift-left-cycle xs 2)
(shift-left-cycle xs -2)
(shift-left-cycle xs 0)
(shift-left-cycle xs 42)
]


@defproc[
(shifts
[lst list?]
[how-far (listof integer?)]
[fill-item any/c #f]
[cycle? boolean? #f])
(listof list?)]
Same as @racket[shift], but @racket[_how-far] is a list of integers rather than a single integer, and the result is a list of lists rather than a single list.

@examples[#:eval my-eval
(define xs (range 5))
(shifts xs '(-2 2))
(shifts xs '(-2 2) 0)
(shifts xs '(-2 2) 'boing)
(shifts xs '(-2 2) 'boing #t)
]

@defproc[
(shift/values
[lst list?]
[how-far (or/c integer? (listof integer?))]
[fill-item any/c #f])
any]
When @racket[_how-far] is a single integer, same as @racket[shift], but the resulting list is returned as values. When @racket[_how-far] is a list of integers, same as @racket[shifts], but the resulting lists are returned as multiple values rather than as a list of lists.

@examples[#:eval my-eval
(define xs (range 5))
(shift xs 1)
(shift/values xs 1)
(shifts xs '(-1 0 1))
(shift/values xs '(-1 0 1))
]

