#lang racket/base
(require racket/contract racket/vector racket/list)
(require "coerce.rkt" "len.rkt")

(provide get in?)


(define/contract (sliceable-container? x)
  (any/c . -> . boolean?)
  (ormap (λ(proc) (proc x)) (list list? string? symbol? vector?)))

(define/contract (gettable-container? x)
  (any/c . -> . boolean?)
  (ormap (λ(proc) (proc x)) (list sliceable-container? hash?))) 



;; general way of fetching an item from a container
(define/contract (get container start [end #f])
  ((gettable-container? any/c) ((λ(i)(or (integer? i) (and (symbol? i) (equal? i 'end))))) 
                               . ->* . any/c)
  
  (set! end
        (if (sliceable-container? container)
            (cond 
              ;; treat negative lengths as offset from end (Python style)
              [(and (integer? end) (< end 0)) (+ (len container) end)]
              ;; 'end slices to the end
              [(equal? end 'end) (len container)]
              ;; default to slice length of 1 (i.e, single-item retrieval)
              [(equal? end #f) (add1 start)]
              [else end])
            end))
  
  (define result (cond
                   ;; for sliceable containers, make a slice
                   [(list? container) (for/list ([i (range start end)]) 
                                        (list-ref container i))]
                   [(vector? container) (for/vector ([i (range start end)])
                                          (vector-ref container i))] 
                   [(string? container) (substring container start end)]
                   [(symbol? container) (->symbol (get (->string container) start end))] 
                   ;; for hash, just get item
                   [(hash? container) (hash-ref container start)]
                   [else #f]))
  
  ;; don't return single-item results inside a list
  (if (and (sliceable-container? container) (= (len result) 1))
      (car (->list result))
      result))




;; general way of testing for membership (à la Python 'in')
;; put item as first arg so function can use infix notation
;; (item . in . container)
(define/contract (in? item container)
  (any/c any/c . -> . boolean?)
  (->boolean (cond
               [(list? container) (member item container)] ; returns #f or sublist beginning with item
               [(vector? container) (vector-member item container)] ; returns #f or zero-based item index
               [(hash? container) 
                (and (hash-has-key? container item) (get container item))] ; returns #f or hash value
               [(string? container) ((->string item) . in? . (map ->string (string->list container)))] ; returns #f or substring beginning with item
               [(symbol? container) ((->string item) . in? . (->string container))] ; returns #f or subsymbol (?!) beginning with item
               [else #f])))