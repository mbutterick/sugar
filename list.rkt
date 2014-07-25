#lang racket/base
(require (for-syntax racket/base))
(require racket/list racket/set)
(require "define.rkt" "len.rkt" "coerce.rkt")

(define+provide/contract (trimf xs test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf xs test-proc) test-proc))


(define+provide/contract (list->slices xs len)
  (list? integer? . -> . (listof list?))
  (cond
    [(equal? xs null) null]
    [(len . > . (length xs)) (list xs)]
    [else (cons (take xs len) (list->slices (drop xs len) len))]))


(define+provide/contract (filter-split xs split-test)
  (list? predicate/c . -> . (listof list?))
  (let loop ([xs (trimf xs split-test)] [acc '()]) 
    (if (empty? xs) 
        (reverse acc)  ; because accumulation is happening backward 
        (let-values ([(item rest) 
                      ;; drop matching elements from front
                      ;; then split on nonmatching 
                      ;; = nonmatching item + other elements (which will start with matching)
                      (splitf-at (dropf xs split-test) (compose1 not split-test))])
          (loop rest (cons item acc))))))


(define+provide/contract (frequency-hash x)
  (list? . -> . hash?)
  (define counter (make-hash))
  (for ([item (flatten x)]) 
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