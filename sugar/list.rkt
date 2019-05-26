#lang racket/base
(require (for-syntax
          racket/base)
         racket/list
         racket/match
         racket/function
         "define.rkt")

(define (increasing-nonnegative-list? x)
  (and (list? x) (or (empty? x) (apply < -1 x))))

(define+provide+safe (trimf xs test-proc)
  (list? procedure? . -> . list?)
  (unless (list? xs)
    (raise-argument-error 'trimf "list?" xs))
  (dropf-right (dropf xs test-proc) test-proc))

(define (slicef-and-filter-split-helper xs pred [separate-negated? #f])
  (let loop ([xs xs][negating? #f][acc empty][negated-acc empty])
    (match xs
      [(? empty?) (if separate-negated?
                      (values (reverse acc) (reverse negated-acc))
                      (reverse acc))]
      [(list* (? (if negating? (negate pred) pred) pred-xs) ... other-xs)
       (cond
         [(and negating? separate-negated?)
          (loop other-xs
                (not negating?)
                acc
                (match pred-xs
                  [(? empty?) negated-acc]
                  [_ (cons pred-xs negated-acc)]))]
         [else
          (loop other-xs
                (not negating?)
                (match pred-xs
                  [(? empty?) acc]
                  [_ (cons pred-xs acc)])
                negated-acc)])])))


(define+provide+safe (slicef xs pred)
  (list? procedure? . -> . (listof list?))
  (unless (list? xs)
    (raise-argument-error 'slicef "list?" xs))
  (slicef-and-filter-split-helper xs pred))

(define+provide+safe (slicef-at xs pred [force? #f])
  ((list? procedure?) (boolean?) . ->* . (listof list?))
  (unless (list? xs)
    (raise-argument-error 'slicef-at "list?" xs))
  (unless (procedure? pred)
    (raise-argument-error 'slicef-at "procedure?" pred))
  (let loop ([xs xs][acc empty])
    (match xs
      [(== empty) (reverse acc)]
      [(list* (? pred pred-x) (? (negate pred) not-pred-xs) ... tail)
       (loop tail (cons (cons pred-x not-pred-xs) acc))]
      [(list* (? (negate pred) not-pred-xs) ... tail)
       (loop tail (if force? acc (cons not-pred-xs acc)))])))

(define+provide+safe (slicef-after xs pred [force? #f])
  ((list? procedure?) (boolean?) . ->* . (listof list?))
  (unless (list? xs)
    (raise-argument-error 'slicef-after "list?" xs))
  (unless (procedure? pred)
    (raise-argument-error 'slicef-after "procedure?" pred))
  (let loop ([xs xs][acc empty])
    (match xs
      [(== empty) (reverse acc)]
      [(list* (? (negate pred) not-pred-xs) ... (? pred pred-x) tail)
       (loop tail (cons (append not-pred-xs (list pred-x)) acc))]
      [tail (loop empty (if force? acc (cons tail acc)))])))

(define+provide+safe (slice-at xs len [force? #f])
  ((list? exact-nonnegative-integer?) (boolean?) . ->* . (listof list?))
  (unless (list? xs)
    (raise-argument-error 'slice-at "list?" xs))
  (unless (and (integer? len) (positive? len))
    (raise-argument-error 'slice-at "positive integer for sublist length" len))
  (let loop ([xs xs][slices empty])
    (if (< (length xs) len)
        (reverse (if (or force? (empty? xs))
                     slices
                     (cons xs slices)))
        (match/values (split-at xs len)
                      [(subxs rest) (loop rest (cons subxs slices))]))))

(define+provide+safe (partition* pred xs)
  (predicate/c list? . -> . (values list? list?))
  (unless (list? xs)
    (raise-argument-error 'partition* "list?" xs))
  (slicef-and-filter-split-helper xs pred 'drop-negated))

(define+provide+safe (filter-split xs pred)
  (list? predicate/c . -> . (listof list?))
  (unless (list? xs)
    (raise-argument-error 'filter-split "list?" xs))
  ;; same idea as slicef, but the negated items are dropped
  (define-values (negated-pred-xs _) (partition* (negate pred) xs))
  negated-pred-xs)

(define+provide+safe (frequency-hash xs)
  (list? . -> . hash?)
  (unless (list? xs)
    (raise-argument-error 'frequency-hash "list?" xs))
  (define counter (make-hash))
  (for ([item (in-list xs)])
    (hash-update! counter item add1 0))
  counter)

(define (->list x)
  (match x
    [(? list? x) x]
    [(? vector?) (vector->list x)]
    [(? string?) (string->list x)]
    [else (raise-argument-error '->list "item that can be converted to list" x)]))

(define+provide+safe (members-unique? x)
  ((or/c list? vector? string?) . -> . boolean?)
  (match (->list x)
    [(? list? x) (= (length (remove-duplicates x)) (length x))]
    [_ (raise-argument-error 'members-unique? "list, vector, or string" x)]))

(define+provide+safe (members-unique?/error x)
  ((or/c list? vector? string?) . -> . boolean?)
  (match (members-unique? x)
    [(== #false)
     (define duplicate-keys (filter values (hash-map (frequency-hash (->list x)) 
                                                     (λ (element freq) (and (> freq 1) element)))))
     (error (string-append "members-unique? failed because " (if (= (length duplicate-keys) 1) 
                                                                 "item isn't"
                                                                 "items aren't") " unique:") duplicate-keys)]
    [result result]))

(provide+safe values->list)
(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ VALUES-EXPR) #'(call-with-values (λ () VALUES-EXPR) list)]))

(define+provide+safe (sublist xs i j)
  (list? exact-nonnegative-integer? exact-nonnegative-integer? . -> . list?)
  (unless (list? xs)
    (raise-argument-error 'sublist "list?" xs))
  (cond
    [(> j (length xs)) (error 'sublist (format "ending index ~a exceeds length of list" j))]
    [(>= j i) (for/list ([(x idx) (in-indexed xs)]
                         #:when (<= i idx (sub1 j)))
                x)]
    [else (raise-argument-error 'sublist (format "starting index larger than ending index" (list i j)))]))

(define+provide+safe (break-at xs bps-in)
  (list? any/c . -> . (listof list?))
  (unless (list? xs)
    (raise-argument-error 'break-at "list" xs))
  (define bps ((if (list? bps-in) values list) bps-in))
  (when (ormap (λ (bp) (<= (length xs) bp)) bps)
    (raise-argument-error 'break-at
                          (format "breakpoints not greater than or equal to input list length = ~a" (length xs)) bps))
  (unless (increasing-nonnegative-list? bps)
    (raise-argument-error 'break-at "increasing-nonnegative-list" bps))
  ;; easier to do back to front, because then the list index for each item won't change during the recursion
  ;; cons a zero onto bps (which may already start with zero) and then use that as the terminating condition
  ;; because breaking at zero means we've reached the start of the list
  (let loop ([xs xs][bps (reverse (cons 0 bps))][acc empty])
    (match bps
      [(cons (? zero?) _) (cons xs acc)] ; return whatever's left, because no more splits are possible
      [_ (match/values (split-at xs (car bps))
                       [(head tail) (loop head (cdr bps) (cons tail acc))])])))

(define (shift-base xs how-far fill-item cycle caller)
  (unless (list? xs)
    (raise-argument-error caller "list?" xs))
  (define abs-how-far (if cycle
                          (modulo (abs how-far) (length xs))
                          (abs how-far)))
  (define (make-fill thing) (if cycle thing (make-list abs-how-far fill-item)))
  (cond 
    [(> abs-how-far (length xs))
     (raise-argument-error caller
                           (format "index not larger than list length ~a" (length xs))
                           (* (if (eq? caller 'shift-left) -1 1) how-far))]
    [(zero? how-far) xs]
    [(positive? how-far)
     (match/values (split-at-right xs abs-how-far)
                   [(head tail) (append (make-fill tail) head)])]
    [else ; how-far is negative
     (match/values (split-at xs abs-how-far)
                   [(head tail) (append tail (make-fill head))])]))

(define+provide+safe (shift xs how-far [fill-item #f] [cycle #f])
  ((list? integer?) (any/c boolean?) . ->* . list?)
  (shift-base xs how-far fill-item cycle 'shift))

(define+provide+safe (shift-left xs how-far [fill-item #f] [cycle #f])
  ((list? integer?) (any/c boolean?) . ->* . list?)
  (shift-base xs (- how-far) fill-item cycle 'shift-left))

(define+provide+safe (shift-cycle xs how-far)
  (list? integer? . -> . list?)
  (shift-base xs how-far #false #true 'shift-cycle))

(define+provide+safe (shift-left-cycle xs how-far)
  (list? integer? . -> . list?)
  (shift-base xs (- how-far) #false #true 'shift-left-cycle))

(define+provide+safe (shifts xs how-fars [fill-item #f] [cycle #f])
  ((list? (listof integer?)) (any/c boolean?) . ->* . (listof list?))
  (unless (list? xs)
    (raise-argument-error 'shifts "list?" xs))
  (map (λ (how-far) (shift xs how-far fill-item cycle)) how-fars))

(define+provide+safe (shift/values xs shift-amount-or-amounts [fill-item #f] [cycle #f])
  ((list? (or/c (listof integer?) integer?)) (any/c boolean?) . ->* . any)
  (apply values ((if (list? shift-amount-or-amounts)
                     shifts
                     shift) xs shift-amount-or-amounts fill-item cycle)))