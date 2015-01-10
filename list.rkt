#lang racket/base
(require (for-syntax racket/base))
(require racket/list racket/set racket/function)
(require "define.rkt" "len.rkt" "coerce.rkt")

(define+provide/contract (trimf xs test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf xs test-proc) test-proc))

(define (list-of-lists? xs) (and (list? xs) (andmap list? xs)))

(define+provide/contract (slicef-at xs pred [force? #f])
  ((list? procedure?) (boolean?) . ->* . list-of-lists?)
  (cond
    [(null? xs) null]
    [force? (slicef-at (dropf xs (compose1 not pred)) pred)]
    [else
     (define-values (car-match others) (splitf-at xs pred))
     (define-values (head tail) (splitf-at others (compose1 not pred)))
     (cons (append (or car-match null) head) (slicef-at tail pred force?))]))


(define+provide/contract (slice-at xs len [force? #f])
  ((list? (and/c integer? positive?)) (boolean?) . ->* . list-of-lists?)
  (cond
    [(equal? xs null) null]
    [(len . > . (length xs)) (if force? null (list xs))]
    [else (cons (take xs len) (slice-at (drop xs len) len force?))]))


(define+provide/contract (filter-split xs split-test)
  (list? predicate/c . -> . list-of-lists?)
  (define-values (last-list list-of-lists)
    (for/fold ([current-list empty][list-of-lists empty])
              ([x (in-list xs)])
      (if (split-test x)
          (values empty (if (not (empty? current-list))
                            (cons (reverse current-list) list-of-lists)
                            list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (reverse (if (not (empty? last-list))
               (cons (reverse last-list) list-of-lists)
               list-of-lists)))


(define+provide/contract (frequency-hash x)
  (list? . -> . hash?)
  (define counter (make-hash))
  (for ([item (in-list (flatten x))]) 
    (hash-set! counter item (add1 (hash-ref counter item 0))))
  counter)


(define+provide/contract (members-unique? x)
  ((or/c list? vector? string?) . -> . boolean?)  
  (cond 
    [(list? x) (= (len (remove-duplicates x)) (len x))]
    [(vector? x) (->list x)]
    [(string? x) (string->list x)]
    [else (error (format "members-unique? cannot be determined for ~a" x))]))



(define+provide/contract (members-unique?/error x)
  (any/c . -> . boolean?)
  (define result (members-unique? x))
  (if (not result)
      (let* ([duplicate-keys (filter-not empty? (hash-map (frequency-hash x) 
                                                          (λ(k v) (if (> v 1) k '()))))])
        (error (string-append "members-unique? failed because " (if (= (len duplicate-keys) 1) 
                                                                    "item isn’t"
                                                                    "items aren’t") " unique:") duplicate-keys))
      result))


;; for use inside quasiquote
;; instead of ,(when ...) use ,@(when/splice ...)
;; to avoid voids
(provide when/splice)
(define-syntax (when/splice stx)
  (syntax-case stx ()
    [(_ test body)
     #'(if test (list body) '())]))


(provide values->list)
(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))


(define+provide/contract (sublist xs i j)
  (list? (and/c integer? (not/c negative?)) (and/c integer? (not/c negative?)) . -> . list?)
  (cond
    [(> j (length xs)) (error 'sublist (format "ending index ~a exceeds length of list" j))]
    [(>= j i) (take (drop xs i) (- j i))]
    [else (error 'sublist (format "starting index ~a is larger than ending index ~a" i j))]))

(define increasing-nonnegative? (λ(xs) (apply < -1 xs)))
(define increasing-nonnegative-list? (and/c list? increasing-nonnegative?))

(define+provide/contract (break-at xs bps)
  (list? (and/c coerce/list? (or/c empty? increasing-nonnegative-list?)) . -> . list-of-lists?)
  (when (ormap (λ(bp) (>= bp (length xs))) bps)
    (error 'break-at (format "breakpoint in ~v is greater than or equal to input list length = ~a" bps (length xs))))
  ;; easier to do back to front, because then the list index for each item won't change during the recursion
  ;; cons a zero onto bps (which may already start with zero) and then use that as the terminating condition
  ;; because breaking at zero means we've reached the start of the list
  (reverse (let loop ([xs xs][bps (reverse (cons 0 bps))])
             (if (= (car bps) 0)
                 (cons xs null) ; return whatever's left, because no more splits are possible
                 (let-values ([(head tail) (split-at xs (car bps))])
                   (cons tail (loop head (cdr bps))))))))


(define (integers? x)
  (and (list? x) (andmap integer? x)))

(define+provide/contract (shift xs shift-amount-or-amounts [fill-item #f] [cycle? #f])
  ((list? (or/c integer? integers?)) (any/c boolean?) . ->* . list?)
  
  (define (do-shift xs how-far)
    (define abs-how-far (abs how-far))
    (cond 
      [(> abs-how-far (length xs)) (error 'shift "index is too large for list\nindex: ~a\nlist: ~v" how-far xs)]
      [(= how-far 0) xs]
      [(positive? how-far) (append (make-list abs-how-far fill-item) (drop-right xs abs-how-far))]
      ;; otherwise how-far is negative
      [else (append (drop xs abs-how-far) (make-list abs-how-far fill-item))]))
  
  (if (list? shift-amount-or-amounts)
      (map (curry do-shift xs) shift-amount-or-amounts)
      (do-shift xs shift-amount-or-amounts)))


(define+provide/contract (shift/values xs shift-amount-or-amounts [fill-item #f])
  ((list? (or/c integer? integers?)) (any/c) . ->* . any)
  (apply (if (list? shift-amount-or-amounts) 
             values
             (λ xs xs)) 
         (shift xs shift-amount-or-amounts fill-item)))


