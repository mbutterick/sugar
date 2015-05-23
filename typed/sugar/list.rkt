#lang typed/racket/base
(require (for-syntax racket/base racket/syntax) racket/function)
(require (except-in racket/list flatten dropf dropf-right) typed/sugar/define "coerce.rkt" "len.rkt")
(require/typed racket/list [dropf (All (A) (Listof A) (A -> Boolean) -> (Listof A))]
               [dropf-right (All (A) (Listof A) (A -> Boolean) -> (Listof A))])
;; use fully-qualified paths in require,
;; so they'll work when this file is included elsewhere
(provide (all-defined-out))

(define/typed+provide (trimf xs test-proc)
  (All (A) ((Listof A) (A -> Boolean) -> (Listof A)))
  (dropf-right (dropf xs test-proc) test-proc))

(define/typed+provide (slicef xs pred)
  (All (A) ((Listof A) (A -> Boolean) -> (Listof (Listof A))))
  (define-values (last-list list-of-lists last-negating)
    (for/fold: ([current-list : (Listof A) empty]
                [list-of-lists : (Listof (Listof A)) empty]
                [negating? : Boolean #f])
      ([x (in-list xs)])
      (define current-pred (if negating? (λ: ([x : A]) (not (pred x))) pred))
      (if (current-pred x)
          (values (cons x current-list) list-of-lists negating?)
          (values (cons x null) (if (not (empty? current-list))
                                    (cons (reverse current-list) list-of-lists)
                                    list-of-lists) (not negating?)))))
  (reverse (cons (reverse last-list) list-of-lists)))


(define/typed+provide (slicef-at xs pred [force? #f])
  ;; with polymorphic function, use cased typing to simulate optional position arguments 
  (All (A) (case-> ((Listof A) (A -> Boolean) -> (Listof (Listof A)))
                   ((Listof A) (A -> Boolean) Boolean -> (Listof (Listof A)))))
  (define-values (last-list list-of-lists)
    (for/fold:
        ([current-list : (Listof A) empty][list-of-lists : (Listof (Listof A)) empty])
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

(define/typed+provide (slicef-after xs pred)
  (All (A) ((Listof A) (A -> Boolean) -> (Listof (Listof A))))
  (define-values (last-list list-of-lists)
    (for/fold: ([current-list : (Listof A) empty][list-of-lists : (Listof (Listof A)) empty])
      ([x (in-list xs)])
      (if (pred x)
          (values empty (cons (reverse (cons x current-list)) list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (reverse (if (empty? last-list)
               list-of-lists
               (cons (reverse last-list) list-of-lists))))


(define/typed+provide (slice-at xs len [force? #f])
  ;; with polymorphic function, use cased typing to simulate optional position arguments 
  (All (A) (case-> ((Listof A) Positive-Integer -> (Listof (Listof A)))
                   ((Listof A) Positive-Integer Boolean -> (Listof (Listof A)))))
  (define-values (last-list list-of-lists)
    (for/fold: ([current-list : (Listof A) empty][list-of-lists : (Listof (Listof A)) empty])
      ([x (in-list xs)][i (in-naturals)])
      (if (= (modulo (add1 i) len) 0)
          (values empty (cons (reverse (cons x current-list)) list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (reverse (if (or (empty? last-list) (and force? (not (= len (length last-list)))))
               list-of-lists
               (cons (reverse last-list) list-of-lists))))


(define/typed+provide (filter-split xs pred)
  (All (A) ((Listof A) (A -> Boolean) -> (Listof (Listof A))))
  (define-values (last-list list-of-lists)
    (for/fold: ([current-list : (Listof A) empty][list-of-lists : (Listof (Listof A)) empty])
      ([x (in-list xs)])
      (if (pred x)
          (values empty (if (not (empty? current-list))
                            (cons (reverse current-list) list-of-lists)
                            list-of-lists))
          (values (cons x current-list) list-of-lists))))
  (reverse (if (not (empty? last-list))
               (cons (reverse last-list) list-of-lists)
               list-of-lists)))

(define/typed+provide (frequency-hash xs)
  (All (A) ((Listof A) -> (HashTable A Integer)))
  (define counter ((inst make-hash A Integer)))
  (for ([item (in-list xs)])
    (hash-update! counter item (λ:([v : Integer]) (add1 v)) (λ _ 0)))
  counter)



(define/typed+provide (members-unique? x)
  (All (A) ((U (Listof A) (Vectorof A) String) -> Boolean))  
  (cond 
    [(list? x) (= (len (remove-duplicates x)) (len x))]
    [(vector? x) (members-unique? (->list x))]
    [(string? x) (members-unique? (string->list x))]
    [else (error (format "members-unique? cannot be determined for ~a" x))]))


(define/typed+provide (members-unique?/error x)
  (All (A) ((U (Listof A) (Vectorof A) String) -> Boolean))
  (define result (members-unique? x))
  (if (not result)
      (let* ([duplicate-keys (filter-not empty? (hash-map (frequency-hash (->list x)) 
                                                          (λ:([element : Any] [freq : Integer]) (if (> freq 1) element '()))))])
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


(define/typed+provide (sublist xs i j)
  (All (A) ((Listof A) Index Index -> (Listof A)))
  (cond
    [(> j (length xs)) (error 'sublist (format "ending index ~a exceeds length of list" j))]
    [(>= j i) (take (drop xs i) (- j i))]
    [else (error 'sublist (format "starting index ~a is larger than ending index ~a" i j))]))


(define/typed+provide (break-at xs bps)
  (All (A) ((Listof A) (U Nonnegative-Integer (Listof Nonnegative-Integer)) -> (Listof (Listof A))))
  (let ([bps (if (list? bps) bps (list bps))]) ; coerce bps to list
    (when (ormap (λ:([bp : Nonnegative-Integer]) (>= bp (length xs))) bps)
      (error 'break-at (format "breakpoint in ~v is greater than or equal to input list length = ~a" bps (length xs))))
    ;; easier to do back to front, because then the list index for each item won't change during the recursion
    ;; cons a zero onto bps (which may already start with zero) and then use that as the terminating condition
    ;; because breaking at zero means we've reached the start of the list
    (reverse (let loop ([xs xs][bps (reverse (cons 0 bps))])
               (if (= (car bps) 0)
                   (cons xs null) ; return whatever's left, because no more splits are possible
                   (let-values ([(head tail) (split-at xs (car bps))])
                     (cons tail (loop head (cdr bps)))))))))


(define/typed+provide (shift xs how-far [fill-item #f] [cycle #f])
  (All (A) (case-> ((Listof (Option A)) Integer -> (Listof (Option A)))
                   ((Listof (Option A)) Integer (Option A) -> (Listof (Option A)))
                   ((Listof (Option A)) Integer (Option A) Boolean -> (Listof (Option A)))))  
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

(define/typed+provide (shifts xs how-fars [fill-item #f] [cycle #f])
  (All (A) (case-> ((Listof (Option A)) (Listof Integer) -> (Listof (Listof (Option A))))
                   ((Listof (Option A)) (Listof Integer) (Option A) -> (Listof (Listof (Option A))))
                   ((Listof (Option A)) (Listof Integer) (Option A) Boolean -> (Listof (Listof (Option A))))))
  (map (λ:([how-far : Integer]) (shift xs how-far fill-item cycle)) how-fars))

