#lang racket/base
(require "define.rkt")
(require "coerce.rkt" "len.rkt" racket/list racket/set racket/sequence racket/stream racket/dict)

(define (sliceable-container? x)
  (ormap (λ(proc) (proc x)) (list list? string? symbol? vector? path? (λ(i) (and (not (dict? i)) (sequence? i))))))

(define (gettable-container? x)
  (ormap (λ(proc) (proc x)) (list sliceable-container? dict?))) 


(define/contract+provide (get container start [end #f])
  ((gettable-container? any/c) ((or/c (and/c integer? positive?) #f)) . ->* . any/c)
  
  (define result 
    ;; use handler to capture error & print localized error message
    (with-handlers ([exn:fail? (λ(exn) (error (format "get: couldn’t retrieve ~a from ~a" (if end (format "items ~a through ~a" start end) (format "item ~a" start)) container)))])
    (let ([end (if (and (equal? end #f) (sliceable-container? container)) (add1 start) end)])
        (cond
          [(list? container) (for/list ([i (range start end)]) (list-ref container i))]
          [(vector? container) (for/vector ([i (range start end)]) (vector-ref container i))]
          [(string? container) (substring container start end)]
          [(symbol? container) (->symbol (get (->string container) start end))] 
          [(path? container) (get (explode-path container) start end)] 
          [(dict? container) (dict-ref container start)]
          [(sequence? container) (get (->list container) start end)]
          [else (error)]))))
  
  ;; don't return single-item results inside a list
  ;; check for integer because integers don't have length
  (if (and (not (integer? result)) (= (len result) 1) (sliceable-container? container))
      (car (->list result))
      result))

(define (listlike-container? container)
  (ormap (λ(pred) (pred container)) (list vector? set? sequence?)))

(define/contract+provide (in? item container)
  (any/c any/c . -> . coerce/boolean?)
  (cond
    [(list? container) (member item container)]
    [(dict? container) (dict-has-key? container item)]
    [(path? container) (in? (->path item) (explode-path container))]
    [(stringish? container) (regexp-match (->string item) (->string container))]
    ;; location relevant because dicts and strings are also listlike (= sequences)
    [(listlike-container? container) (in? item (->list container))]
    [else #f]))
