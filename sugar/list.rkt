#lang racket/base
(require (for-syntax racket/base) racket/list "define.rkt")

(define (list-of-lists? xs) (and (list? xs) (andmap list? xs)))
(define (index? x) (and (integer? x) (not (negative? x))))

(define increasing-nonnegative-list? (λ(x) (and (list? x) (or (empty? x)
                                                              (apply < -1 x)))))

(define (integers? x) (and (list? x) (andmap integer? x)))

(define (negate pred) (λ(x) (not (pred x))))

(define+provide+safe (trimf xs test-proc)
  (list? procedure? . -> . list?)
  (unless (list? xs)
    (raise-argument-error 'trimf "list?" xs))
  (dropf-right (dropf xs test-proc) test-proc))


(define (slicef-and-filter-split-helper xs pred [drop-negated? #f])
  (let loop ([xs xs][negating? #f])
    (cond
      [(empty? xs) empty]
      [else
       (define loop-pred (if negating? (negate pred) pred))
       (define-values (loop-pred-xs other-xs) (splitf-at xs loop-pred))
       (define subxs (if (and negating? drop-negated?) empty loop-pred-xs))
       (if (empty? subxs)
           (loop other-xs (not negating?))
           (cons subxs (loop other-xs (not negating?))))])))


(define+provide+safe (slicef xs pred)
  (list? procedure? . -> . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'slicef "list?" xs))
  (slicef-and-filter-split-helper xs pred))


(define+provide+safe (slicef-at xs pred [force? #f])
  ((list? procedure?) (boolean?) . ->* . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'slicef-at "list?" xs))
  (let loop ([xs xs])
    (cond
      [(empty? xs) empty]
      [(pred (car xs))
       (define-values (not-pred-xs rest) (splitf-at (cdr xs) (negate pred)))
       (cons (cons (car xs) not-pred-xs) (loop rest))]
      [else
       (define-values (not-pred-xs rest) (splitf-at xs (negate pred)))
       (if force?
           (loop rest)
           (cons not-pred-xs (loop rest)))])))


(define+provide+safe (slicef-after xs pred)
  (list? procedure? . -> . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'slicef-after "list?" xs))
  (let loop ([xs xs])
    (cond
      [(empty? xs) empty]
      [else
       (define-values (not-pred-xs rest) (splitf-at xs (negate pred)))
       (if (pair? rest)
           (let ([must-be-pred-x (car rest)])
             (cons (append not-pred-xs (list must-be-pred-x)) (loop (cdr rest))))
           not-pred-xs)])))


(define+provide+safe (slice-at xs len [force? #f])
  ((list? (and/c integer? positive?)) (boolean?) . ->* . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'slice-at "list?" xs))
  (unless (and (integer? len) (positive? len))
    (raise-argument-error 'slice-at "positive integer for sublist length" len))
  (let loop ([xs xs])
    (cond
      [(< (length xs) len) (if (or force? (empty? xs)) empty (list xs))]
      [else
       (define-values (subxs rest) (split-at xs len))
       (cons subxs (loop rest))])))


(define+provide+safe (filter-split xs pred)
  (list? predicate/c . -> . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'filter-split "list?" xs))
  ;; same idea as slicef, but the negated items are dropped(-
  (slicef-and-filter-split-helper xs (negate pred) 'drop-negated))


(define+provide+safe (frequency-hash xs)
  (list? . -> . hash?)
  (unless (list? xs)
    (raise-argument-error 'frequency-hash "list?" xs))
  (define counter (make-hash))
  (for ([item (in-list xs)])
    (hash-update! counter item add1 0))
  counter)

(define (->list x)
  (cond
    [(list? x) x]
    [(vector? x) (vector->list x)]
    [(string? x) (string->list x)]
    [else (error '->list)]))


(define+provide+safe (members-unique? x)
  ((or/c list? vector? string?) . -> . boolean?)
  (let ([x (->list x)])
    (cond 
      [(list? x) (= (length (remove-duplicates x)) (length x))]
      [else (error (format "members-unique? cannot be determined for ~a" x))])))


(define+provide+safe (members-unique?/error x)
  ((or/c list? vector? string?) . -> . boolean?)
  (define result (members-unique? x))
  (if (not result)
      (let* ([duplicate-keys (filter-not empty? (hash-map (frequency-hash (->list x)) 
                                                          (λ(element freq) (if (> freq 1) element '()))))])
        (error (string-append "members-unique? failed because " (if (= (length duplicate-keys) 1) 
                                                                    "item isn't"
                                                                    "items aren't") " unique:") duplicate-keys))
      result))


(provide+safe values->list)
(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))


(define+provide+safe (sublist xs i j)
  (list? index? index? . -> . list?)
  (unless (list? xs)
    (raise-argument-error 'sublist "list?" xs))
  (cond
    [(> j (length xs)) (error 'sublist (format "ending index ~a exceeds length of list" j))]
    [(>= j i) (take (drop xs i) (- j i))]
    [else (error 'sublist (format "starting index ~a is larger than ending index ~a" i j))]))


(define+provide+safe (break-at xs bps)
  (list? any/c . -> . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'break-at "list?" xs))
  (let ([bps (if (list? bps) bps (list bps))]) ; coerce bps to list
    (when (ormap (λ(bp) (>= bp (length xs))) bps)
      (error 'break-at (format "breakpoint in ~v is greater than or equal to input list length = ~a" bps (length xs))))
    (when (not (increasing-nonnegative-list? bps))
      (raise-argument-error 'break-at "increasing-nonnegative-list?" bps))
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
  (unless (list? xs)
    (raise-argument-error 'shift "list?" xs))
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
  (unless (list? xs)
    (raise-argument-error 'shifts "list?" xs))
  (map (λ(how-far) (shift xs how-far fill-item cycle)) how-fars))


;; todo: can this work in typed context? couldn't figure out how to polymorphically `apply values`
;; macro doesn't work either
(define+provide+safe (shift/values xs shift-amount-or-amounts [fill-item #f] [cycle #f])
  ((list? (or/c integers? integer?)) (any/c boolean?) . ->* . any)
  (apply values ((if (list? shift-amount-or-amounts)
                     shifts
                     shift) xs shift-amount-or-amounts fill-item cycle)))