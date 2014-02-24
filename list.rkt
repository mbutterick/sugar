#lang racket/base
(require racket/contract racket/list racket/set)
(require "define.rkt" "len.rkt" "coerce.rkt")

;; trim from beginning & end of list
(define+provide/contract (trim items test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf items test-proc) test-proc))

;; convert a list into a list of slices that are len long (last one might be shorter)
(define+provide/contract (list->slices xs len)
  (list? integer? . -> . (listof list?))
  (cond
    [(equal? xs null) null]
    [(len . > . (length xs)) (list xs)]
    [else (cons (take xs len) (list->slices (drop xs len) len))]))




;; split list into list of sublists using test-proc
(define+provide/contract (splitf-at* xs split-test)
  
  ;; todo: better error message when split-test is not a predicate
  (list? predicate/c . -> . (listof list?))
  (define (&splitf-at* xs [acc '()]) ; use acc for tail recursion
    (if (empty? xs) 
        ;; reverse because accumulation is happening backward 
        ;; (because I'm using cons to push latest match onto front of list)
        (reverse acc)
        (let-values ([(item rest) 
                      ;; drop matching elements from front
                      ;; then split on nonmatching 
                      ;; = nonmatching item + other elements (which will start with matching)
                      (splitf-at (dropf xs split-test) (compose1 not split-test))])
          ;; recurse, and store new item in accumulator
          (&splitf-at* rest (cons item acc)))))
  
  ;; trim off elements matching split-test
  (&splitf-at* (trim xs split-test)))


;; count incidence of elements in a list
;; returns hash where key is element, value is incidence
;; todo: move this? Ideally it would be in tools,
;; but that would create a circular dependency.
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