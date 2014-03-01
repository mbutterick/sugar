#lang racket/base
(require "define/contract.rkt")
(require "coerce.rkt" "len.rkt" racket/list)

(define (sliceable-container? x)
  (ormap (λ(proc) (proc x)) (list list? string? symbol? vector?)))

(define (gettable-container? x)
  (ormap (λ(proc) (proc x)) (list sliceable-container? hash?))) 


(define+provide/contract (get container start [end #f])
  ((gettable-container? any/c) ((λ(i)(or (integer? i) (and (symbol? i) (equal? i 'end))))) . ->* . any/c)
  
  (define result 
    (with-handlers ([exn:fail? (λ(exn) (error (format "Couldn't get item from ~a" container)))])
      (let ([end (if (sliceable-container? container)
                     (cond 
                       ;; treat negative lengths as offset from end (Python style)
                       [(and (integer? end) (< end 0)) (+ (len container) end)]
                       ;; 'end slices to the end
                       [(equal? end 'end) (len container)]
                       ;; default to slice length of 1 (i.e, single-item retrieval)
                       [(equal? end #f) (add1 start)]
                       [else end])
                     end)])
        (cond
          [(list? container) (for/list ([i (range start end)]) (list-ref container i))]
          [(vector? container) (for/vector ([i (range start end)]) (vector-ref container i))] 
          [(string? container) (substring container start end)]
          [(symbol? container) (->symbol (get (->string container) start end))] 
          [(hash? container) (hash-ref container start)]
          [else (error)]))))
  
  ;; don't return single-item results inside a list
  (if (and (sliceable-container? container) (= (len result) 1))
      (car (->list result))
      result))


(define+provide/contract (in? item container)
  (any/c any/c . -> . coerce/boolean?)
  (cond
    [(list? container) (member item container)] ; returns #f or sublist beginning with item
    [(vector? container) (member item (vector->list container))] ; returns #f or sublist beginning with item
    [(hash? container) (and (hash-has-key? container item) (get container item))] ; returns #f or hash value
    [(string? container) (regexp-match (->string item) (->string container))] ; returns #f or substring beginning with item
    [(symbol? container) ((->string item) . in? . (->string container))] ; returns #f or subsymbol (?!) beginning with item
    [else #f]))