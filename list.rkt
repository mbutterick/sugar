#lang racket/base
(require racket/list racket/set)
(require "define/contract.rkt" "len.rkt" "coerce/values.rkt")

(define+provide/contract (trim items test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf items test-proc) test-proc))


(define+provide/contract (list->slices xs len)
  (list? integer? . -> . (listof list?))
  (cond
    [(equal? xs null) null]
    [(len . > . (length xs)) (list xs)]
    [else (cons (take xs len) (list->slices (drop xs len) len))]))


(define+provide/contract (splitf-at* xs split-test)
  (list? predicate/c . -> . (listof list?))
  (let loop ([xs (trim xs split-test)] [acc '()]) 
    (if (empty? xs) 
        (reverse acc)  ; because accumulation is happening backward 
        (let-values ([(item rest) 
                      ;; drop matching elements from front
                      ;; then split on nonmatching 
                      ;; = nonmatching item + other elements (which will start with matching)
                      (splitf-at (dropf xs split-test) (compose1 not split-test))])
          (loop rest (cons item acc))))))


(define+provide/contract (count-incidence x)
  (list? . -> . hash?)
  (define counter (make-hash))
  (for ([item (flatten x)]) 
    (hash-set! counter item (add1 (hash-ref counter item 0))))
  counter)


(define+provide/contract (members-unique? x)
  (any/c . -> . boolean?)
  (cond 
    [(list? x) (= (len (apply set x)) (len x))]
    [(vector? x) (members-unique? (->list x))]
    [(string? x) (members-unique? (string->list x))]
    [else (error (format "members-unique cannot be determined for ~a" x))]))


(define+provide/contract (members-unique?/error x)
  (any/c . -> . boolean?)
  (define result (members-unique? x))
  (if (not result)
      (let* ([duplicate-keys (filter-not empty? (hash-map (count-incidence x) 
                                                          (λ(k v) (if (> v 1) k '()))))])
        (error (string-append (if (= (len duplicate-keys) 1) 
                                  "Item isn’t"
                                  "Items aren’t") " unique:") duplicate-keys))
      result))