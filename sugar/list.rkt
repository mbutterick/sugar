#lang racket/base
(require (for-syntax racket/base)
         racket/list racket/set racket/function sugar/define)
(require "len.rkt" "coerce.rkt")

(require-via-wormhole "../typed/sugar/list.rkt")

(define (list-of-lists? xs) (and (list? xs) (andmap list? xs)))
(define (index? x) (and (integer? x) (not (negative? x))))

(define increasing-nonnegative? (λ(xs) (apply < -1 xs)))
(define increasing-nonnegative-list? (and/c list? increasing-nonnegative?))

(define (integers? x) (and (list? x) (andmap integer? x)))

(provide+safe [trimf (list? procedure? . -> . list?)]
              [slicef-at ((list? procedure?) (boolean?) . ->* . list-of-lists?)]
              [slicef-after (list? procedure? . -> . list-of-lists?)]
              [slice-at ((list? (and/c integer? positive?)) (boolean?) . ->* . list-of-lists?)]
              [filter-split (list? predicate/c . -> . list-of-lists?)]
              [frequency-hash (list? . -> . hash?)]
              [members-unique? ((or/c list? vector? string?) . -> . boolean?)]
              [members-unique?/error ((or/c list? vector? string?) . -> . boolean?)]
              when/splice
              values->list
              [sublist (list? index? index? . -> . list?)]
              [break-at (list? (and/c coerce/list? (or/c empty? increasing-nonnegative-list?)) . -> . list-of-lists?)]
              [shift ((list? (or/c integer? integers?)) (any/c boolean?) . ->* . list?)]
              [shift/values ((list? (or/c integer? integers?)) (any/c) . ->* . any)])


;; todo: can this work in typed context? couldn't figure out how to polymorphically `apply values`
;; macro doesn't work either
(define (shift/values xs shift-amount-or-amounts [fill-item #f])
  (apply (if (list? shift-amount-or-amounts) 
             values
             (λ xs xs)) 
         (shift xs shift-amount-or-amounts fill-item)))

