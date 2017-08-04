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
  (let loop ([xs xs][negating? #f][acc empty])
    (cond
      [(empty? xs) (reverse acc)]
      [else
       (define loop-pred (if negating? (negate pred) pred))
       (define-values (loop-pred-xs other-xs) (splitf-at xs loop-pred))
       (define subxs (if (and negating? drop-negated?) empty loop-pred-xs))
       (loop other-xs (not negating?) (if (empty? subxs) acc (cons subxs acc)))])))


(define+provide+safe (slicef xs pred)
  (list? procedure? . -> . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'slicef "list?" xs))
  (slicef-and-filter-split-helper xs pred))


(define+provide+safe (slicef-at xs pred [force? #f])
  ((list? procedure?) (boolean?) . ->* . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'slicef-at "list?" xs))
  (let loop ([xs xs][acc empty])
    (cond
      [(empty? xs) (reverse acc)]
      [(pred (car xs))
       (define-values (not-pred-xs rest) (splitf-at (cdr xs) (negate pred)))
       (loop rest (cons (cons (car xs) not-pred-xs) acc))]
      [else
       (define-values (not-pred-xs rest) (splitf-at xs (negate pred)))
       (loop rest (if force? acc (cons not-pred-xs acc)))])))

(define+provide+safe (slicef-after xs pred)
  (list? procedure? . -> . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'slicef-after "list?" xs))
  (let loop ([xs xs][acc empty])
    (cond
      [(empty? xs) (reverse acc)]
      [else
       (define-values (not-pred-xs rest) (splitf-at xs (negate pred)))
       (if (pair? rest)
           (let ([must-be-pred-x (car rest)])
             (loop (cdr rest) (cons (append not-pred-xs (list must-be-pred-x)) acc)))
           not-pred-xs)])))


(define+provide+safe (slice-at xs len [force? #f])
  ((list? (and/c integer? positive?)) (boolean?) . ->* . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'slice-at "list?" xs))
  (unless (and (integer? len) (positive? len))
    (raise-argument-error 'slice-at "positive integer for sublist length" len))
  (let loop ([xs xs][slices empty])
    (cond
      [(< (length xs) len) (reverse
                            (if (or force? (empty? xs))
                                slices
                                (cons xs slices)))]
      [else
       (define-values (subxs rest) (split-at xs len))
       (loop rest (cons subxs slices))])))


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
    [(>= j i) (for/list ([(x idx) (in-indexed xs)]
                         #:when (<= i idx (sub1 j)))
                        x)]
    [else (error 'sublist (format "starting index ~a is larger than ending index ~a" i j))]))


(define+provide+safe (break-at xs bps)
  (list? any/c . -> . list-of-lists?)
  (unless (list? xs)
    (raise-argument-error 'break-at "list?" xs))
  (let ([bps (if (list? bps) bps (list bps))]) ; coerce bps to list
    (when (ormap (λ (bp) (>= bp (length xs))) bps)
      (raise-argument-error 'break-at
                            (format "breakpoints not greater than or equal to input list length = ~a" (length xs)) bps))
    (when (not (increasing-nonnegative-list? bps))
      (raise-argument-error 'break-at "increasing-nonnegative-list?" bps))
    ;; easier to do back to front, because then the list index for each item won't change during the recursion
    ;; cons a zero onto bps (which may already start with zero) and then use that as the terminating condition
    ;; because breaking at zero means we've reached the start of the list
    (let loop ([xs xs][bps (reverse (cons 0 bps))][acc empty])
      (if (zero? (car bps))
          (cons xs acc) ; return whatever's left, because no more splits are possible
          (let-values ([(head tail) (split-at xs (car bps))])
            (loop head (cdr bps) (cons tail acc)))))))


(define (shift-base xs how-far fill-item cycle caller)
  (unless (list? xs)
    (raise-argument-error caller "list?" xs))
  (define abs-how-far (if cycle
                          (modulo (abs how-far) (length xs))
                          (abs how-far)))
  (cond 
    [(> abs-how-far (length xs))
     (raise-argument-error caller
                           (format "index not larger than list length ~a" (length xs))
                           (* (if (eq? caller 'shift-left) -1 1) how-far))]
    [(= how-far 0) xs]
    [(positive? how-far)
     (define-values (head tail) (split-at-right xs abs-how-far))
     (define filler (if cycle
                        tail
                        (make-list abs-how-far fill-item)))            
     (append filler head)]
    [else ; how-far is negative
     (define-values (head tail) (split-at xs abs-how-far))
     (define filler (if cycle
                        head
                        (make-list abs-how-far fill-item)))
     (append tail filler)]))


(define+provide+safe (shift xs how-far [fill-item #f] [cycle #f])
  ((list? integer?) (any/c boolean?) . ->* . list?)
  (shift-base xs how-far fill-item cycle 'shift))


(define+provide+safe (shift-left xs how-far [fill-item #f] [cycle #f])
  ((list? integer?) (any/c boolean?) . ->* . list?)
  (shift-base xs (- how-far) fill-item cycle 'shift-left))


(define+provide+safe (shift-cycle xs how-far)
  (list? integer? . -> . list?)
  (shift-base xs how-far #f #t 'shift-cycle))


(define+provide+safe (shift-left-cycle xs how-far)
  (list? integer? . -> . list?)
  (shift-base xs (- how-far) #f #t 'shift-left-cycle))


(define+provide+safe (shifts xs how-fars [fill-item #f] [cycle #f])
  ((list? integers?) (any/c boolean?) . ->* . (listof list?))
  (unless (list? xs)
    (raise-argument-error 'shifts "list?" xs))
  (map (λ(how-far) (shift xs how-far fill-item cycle)) how-fars))


(define+provide+safe (shift/values xs shift-amount-or-amounts [fill-item #f] [cycle #f])
  ((list? (or/c integers? integer?)) (any/c boolean?) . ->* . any)
  (apply values ((if (list? shift-amount-or-amounts)
                     shifts
                     shift) xs shift-amount-or-amounts fill-item cycle)))