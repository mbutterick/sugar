#lang racket/base
(require (for-syntax racket/base) racket/list racket/set racket/function)
(require "len.rkt" "coerce.rkt" "define.rkt")

(define (list-of-lists? xs) (and (list? xs) (andmap list? xs)))
(define (index? x) (and (integer? x) (not (negative? x))))

(define increasing-nonnegative? (λ(xs) (apply < -1 xs)))
(define increasing-nonnegative-list? (and/c list? increasing-nonnegative?))

(define (integers? x) (and (list? x) (andmap integer? x)))


(define+provide+safe (trimf xs test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf xs test-proc) test-proc))


(define+provide+safe (slicef xs pred)
  (list? procedure? . -> . list-of-lists?)
  (define-values (last-list list-of-lists last-negating)
    (for/fold ([current-list empty]
               [list-of-lists empty]
               [negating? #f])
              ([x (in-list xs)])
      (define current-pred (if negating? (λ (x) (not (pred x))) pred))
      (if (current-pred x)
          (values (cons x current-list) list-of-lists negating?)
          (values (cons x null) (if (not (empty? current-list))
                                    (cons (reverse current-list) list-of-lists)
                                    list-of-lists) (not negating?)))))
  (reverse (cons (reverse last-list) list-of-lists)))


(define+provide+safe (slicef-at xs pred [force? #f])
  ;; with polymorphic function, use cased typing to simulate optional position arguments 
  ((list? procedure?) (boolean?) . ->* . list-of-lists?)
  (define-values (last-list list-of-lists)
    (for/fold
     ([current-list empty][list-of-lists empty])
     ([x (in-list xs)])
      (if (pred x)
          (values (cons x null) (if (not (empty? current-list))
                                    (cons (reverse current-list) list-of-lists)
                                    list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (let ([list-of-lists (reverse (if (empty? last-list)
                                    list-of-lists
                                    (cons (reverse last-list) list-of-lists)))])
    (if (and force? (not (empty? list-of-lists)) (not (pred (caar list-of-lists))))
        (cdr list-of-lists)
        list-of-lists)))


(define+provide+safe (slicef-after xs pred)
  (list? procedure? . -> . list-of-lists?)
  (define-values (last-list list-of-lists)
    (for/fold ([current-list empty][list-of-lists empty])
              ([x (in-list xs)])
      (if (pred x)
          (values empty (cons (reverse (cons x current-list)) list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (reverse (if (empty? last-list)
               list-of-lists
               (cons (reverse last-list) list-of-lists))))


(define+provide+safe (slice-at xs len [force? #f])
  ;; with polymorphic function, use cased typing to simulate optional position arguments 
  ((list? (and/c integer? positive?)) (boolean?) . ->* . list-of-lists?)
  (define-values (last-list list-of-lists)
    (for/fold ([current-list empty][list-of-lists empty])
              ([x (in-list xs)][i (in-naturals)])
      (if (= (modulo (add1 i) len) 0)
          (values empty (cons (reverse (cons x current-list)) list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (reverse (if (or (empty? last-list) (and force? (not (= len (length last-list)))))
               list-of-lists
               (cons (reverse last-list) list-of-lists))))


(define+provide+safe (filter-split xs pred)
  (list? predicate/c . -> . list-of-lists?)
  (define-values (last-list list-of-lists)
    (for/fold ([current-list empty][list-of-lists empty])
              ([x (in-list xs)])
      (if (pred x)
          (values empty (if (not (empty? current-list))
                            (cons (reverse current-list) list-of-lists)
                            list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (reverse (if (not (empty? last-list))
               (cons (reverse last-list) list-of-lists)
               list-of-lists)))


(define+provide+safe (frequency-hash xs)
  (list? . -> . hash?)
  (define counter (make-hash))
  (for ([item (in-list xs)])
       (hash-update! counter item (λ(v) (add1 v)) (λ _ 0)))
  counter)


(define+provide+safe (members-unique? x)
  ((or/c list? vector? string?) . -> . boolean?)  
  (cond 
    [(list? x) (= (len (remove-duplicates x)) (len x))]
    [(vector? x) (members-unique? (->list x))]
    [(string? x) (members-unique? (string->list x))]
    [else (error (format "members-unique? cannot be determined for ~a" x))]))


(define+provide+safe (members-unique?/error x)
  ((or/c list? vector? string?) . -> . boolean?)
  (define result (members-unique? x))
  (if (not result)
      (let* ([duplicate-keys (filter-not empty? (hash-map (frequency-hash (->list x)) 
                                                          (λ(element freq) (if (> freq 1) element '()))))])
        (error (string-append "members-unique? failed because " (if (= (len duplicate-keys) 1) 
                                                                    "item isn’t"
                                                                    "items aren’t") " unique:") duplicate-keys))
      result))


;; for use inside quasiquote
;; instead of ,(when ...) use ,@(when/splice ...)
;; to avoid voids
(provide+safe  when/splice)
(define-syntax (when/splice stx)
  (syntax-case stx ()
    [(_ test body)
     #'(if test (list body) '())]))


(provide+safe values->list)
(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))


(define+provide+safe (sublist xs i j)
  (list? index? index? . -> . list?)
  (cond
    [(> j (length xs)) (error 'sublist (format "ending index ~a exceeds length of list" j))]
    [(>= j i) (take (drop xs i) (- j i))]
    [else (error 'sublist (format "starting index ~a is larger than ending index ~a" i j))]))


(define+provide+safe (break-at xs bps)
  (list? (and/c coerce/list? (or/c empty? increasing-nonnegative-list?)) . -> . list-of-lists?)
  (let ([bps (if (list? bps) bps (list bps))]) ; coerce bps to list
    (when (ormap (λ(bp) (>= bp (length xs))) bps)
      (error 'break-at (format "breakpoint in ~v is greater than or equal to input list length = ~a" bps (length xs))))
    ;; easier to do back to front, because then the list index for each item won't change during the recursion
    ;; cons a zero onto bps (which may already start with zero) and then use that as the terminating condition
    ;; because breaking at zero means we've reached the start of the list
    (reverse (let loop ([xs xs][bps (reverse (cons 0 bps))])
               (if (= (car bps) 0)
                   (cons xs null) ; return whatever's left, because no more splits are possible
                   (let-values ([(head tail) (split-at xs (car bps))])
                     (cons tail (loop head (cdr bps)))))))))


(define+provide+safe (shift xs how-far [fill-item #f] [cycle #f])
  ((list? integer?) (any/c boolean?) . ->* . list?)  
  (define abs-how-far (abs how-far))
  (cond 
    [(> abs-how-far (length xs)) (error 'shift "index is too large for list\nindex: ~a\nlist: ~v" how-far xs)]
    [(= how-far 0) xs]
    [(positive? how-far)
     (define filler (if cycle
                        (take-right xs abs-how-far)
                        (make-list abs-how-far fill-item)))            
     (append filler (drop-right xs abs-how-far))]
    [else ; how-far is negative
     (define filler (if cycle
                        (take xs abs-how-far)
                        (make-list abs-how-far fill-item)))
     (append (drop xs abs-how-far) filler)]))


(define+provide+safe (shifts xs how-fars [fill-item #f] [cycle #f])
  ((list? integers?) (any/c boolean?) . ->* . (listof list?))
  (map (λ(how-far) (shift xs how-far fill-item cycle)) how-fars))


;; todo: can this work in typed context? couldn't figure out how to polymorphically `apply values`
;; macro doesn't work either
(define+provide+safe (shift/values xs shift-amount-or-amounts [fill-item #f] [cycle #f])
  ((list? (or/c integers? integer?)) (any/c boolean?) . ->* . any)
  (apply values ((if (list? shift-amount-or-amounts)
                     shifts
                     shift) xs shift-amount-or-amounts fill-item cycle)))