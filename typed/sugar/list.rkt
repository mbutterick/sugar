#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))
(require (except-in racket/list flatten dropf dropf-right) typed/sugar/define "coerce.rkt" "len.rkt")
(require/typed racket/list [dropf (All (A) (Listof A) (A -> Boolean) -> (Listof A))]
               [dropf-right (All (A) (Listof A) (A -> Boolean) -> (Listof A))])
;; use fully-qualified paths in require,
;; so they'll work when this file is included elsewhere
(provide (all-defined-out))

(define/typed+provide (trimf xs test-proc)
  (All (A) ((Listof A) (A -> Boolean) -> (Listof A)))
  (dropf-right (dropf xs test-proc) test-proc))

(define/typed+provide slicef-at
  ;; with polymorphic function, use cased typing to simulate optional position arguments 
  (All (A) (case-> ((Listof A) (A -> Boolean) -> (Listof (Listof A)))
                   ((Listof A) (A -> Boolean) Boolean -> (Listof (Listof A)))))
  (case-lambda
    [(xs pred)
     (slicef-at xs pred #f)]
    [(xs pred force?)
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
           list-of-lists))]))

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


(define/typed+provide slice-at
  ;; with polymorphic function, use cased typing to simulate optional position arguments 
  (All (A) (case-> ((Listof A) Positive-Integer -> (Listof (Listof A)))
                   ((Listof A) Positive-Integer Boolean -> (Listof (Listof A)))))
  (case-lambda
    [(xs len)
     (slice-at xs len #f)]
    [(xs len force?)
     (define-values (last-list list-of-lists)
       (for/fold: ([current-list : (Listof A) empty][list-of-lists : (Listof (Listof A)) empty])
         ([x (in-list xs)][i (in-naturals)])
         (if (= (modulo (add1 i) len) 0)
             (values empty (cons (reverse (cons x current-list)) list-of-lists))
             (values (cons x current-list) list-of-lists))))
     (reverse (if (or (empty? last-list) (and force? (not (= len (length last-list)))))
                  list-of-lists
                  (cons (reverse last-list) list-of-lists)))]))


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
  (All (A) ((Listof A) (U Index (Listof Index)) -> (Listof (Listof A))))
  (let ([bps (if (list? bps) bps (list bps))]) ; coerce bps to list
    (when (ormap (λ:([bp : Index]) (>= bp (length xs))) bps)
      (error 'break-at (format "breakpoint in ~v is greater than or equal to input list length = ~a" bps (length xs))))
    ;; easier to do back to front, because then the list index for each item won't change during the recursion
    ;; cons a zero onto bps (which may already start with zero) and then use that as the terminating condition
    ;; because breaking at zero means we've reached the start of the list
    (reverse (let loop ([xs xs][bps (reverse (cons 0 bps))])
               (if (= (car bps) 0)
                   (cons xs null) ; return whatever's left, because no more splits are possible
                   (let-values ([(head tail) (split-at xs (car bps))])
                     (cons tail (loop head (cdr bps)))))))))


(define/typed+provide shift
  (case-> ((Listof Any) (U Integer (Listof Integer)) -> (Listof Any))
          ((Listof Any) (U Integer (Listof Integer)) Any -> (Listof Any))
          ((Listof Any) (U Integer (Listof Integer)) Any Boolean -> (Listof Any)))  
  (case-lambda
    [(xs shift-amount-or-amounts)
     (shift xs shift-amount-or-amounts #f #f)]
    [(xs shift-amount-or-amounts fill-item)
     (shift xs shift-amount-or-amounts fill-item #f)]
    [(xs shift-amount-or-amounts fill-item cycle)
     (define/typed (do-shift xs how-far)
       ((Listof Any) Integer -> (Listof Any))
       (define abs-how-far (abs how-far))
       (cond 
         [(> abs-how-far (length xs)) (error 'shift "index is too large for list\nindex: ~a\nlist: ~v" how-far xs)]
         [(= how-far 0) xs]
         [(positive? how-far)
          (append  (make-list abs-how-far fill-item) (drop-right xs abs-how-far))]
         ;; otherwise how-far is negative
         [else  (append (drop xs abs-how-far) (make-list abs-how-far fill-item))]))
     (if (list? shift-amount-or-amounts)
         (map (λ:([amount : Integer]) (do-shift xs amount)) shift-amount-or-amounts)
         (do-shift xs shift-amount-or-amounts))]))
