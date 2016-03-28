#lang racket/base
(require "../define.rkt" "../coerce.rkt" "len.rkt" racket/list racket/set racket/sequence racket/stream racket/dict)

(define (sliceable-container? x)
  (ormap (λ(proc) (proc x)) (list list? string? symbol? vector? path? (λ(i) (and (not (dict? i)) (sequence? i))))))

(define (gettable-container? x)
  (ormap (λ(proc) (proc x)) (list sliceable-container? dict?))) 


(define+provide+safe (get container start [end #f])
  ((gettable-container? any/c) ((or/c (and/c integer? positive?) #f)) . ->* . any)
  
  (define result 
    ;; use handler to capture error & print localized error message
    (with-handlers ([exn:fail? (λ(exn) (error (format "get: couldn't retrieve ~a from ~a" (if end (format "items ~a through ~a" start end) (format "item ~a" start)) container)))])
      (let ([end (if (and (equal? end #f) (sliceable-container? container)) (add1 start) end)])
        (cond
          [(list? container) (for/list ([i (in-range start end)]) (list-ref container i))]
          [(vector? container) (for/vector ([i (in-range start end)]) (vector-ref container i))]
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

(define+provide+safe (in? item container)
  (any/c any/c . -> . boolean?)
  (->boolean (cond
               [(list? container) (member item container)]
               [(dict? container) (dict-has-key? container item)]
               [(path? container) (in? (->path item) (explode-path container))]
               [(stringish? container) (regexp-match (->string item) (->string container))]
               ;; location relevant because dicts and strings are also listlike (= sequences)
               [(listlike-container? container) (in? item (->list container))]
               [else #f])))



(module+ test
  (require rackunit)
  (check-equal? (get '(0 1 2 3 4 5) 2) 2)
  (check-exn exn:fail? (λ() (get '(0 1 2 3 4 5) 100))) ; index too big
  (check-equal? (get `(0 1 ,(list 2) 3 4 5) 2) (list 2))
  (check-equal? (get '(0 1 2 3 4 5) 0 2) '(0 1))
  (check-equal? (get (list->vector '(0 1 2 3 4 5)) 2) 2)
  (check-equal? (get (list->vector'(0 1 2 3 4 5)) 0 2) (list->vector '(0 1)))
  (check-equal? (get "purple" 2) "r")
  (check-equal? (get "purple" 0 2) "pu")
  (check-equal? (get 'purple 2) 'r)
  (check-equal? (get 'purple 0 2) 'pu)
  (check-equal? (get (string->path "/root/foo/bar/file.txt") 2) (string->path "foo"))
  (check-equal? (get (string->path "/root/foo/bar/file.txt") 0 2) (list (string->path "/") (string->path "root")))
  (check-equal? (get (make-hash `((a . ,(list 1)) (b . ,(list 2)) (c  . ,(list 3)))) 'a) (list 1))
  (check-exn exn:fail? (λ() (get (make-hash `((a . ,(list 1)) (b . ,(list 2)) (c  . ,(list 3)))) 'z))) ; nonexistent key
  
  (check-equal? (get (string->path "/root/foo/bar/file.txt") 1) (string->path "root"))
  (check-equal? (get (string->path "/root/foo/bar/file.txt") 0 3)
                (map string->path '("/" "root" "foo")))
  
  (check-equal? (get (make-hash '((a . 1) (b . 2) (c  . 3))) 'b) 2)
  
  (check-true (2 . in? . '(1 2 3)))
  (check-false (4 . in? . '(1 2 3)))
  (check-true (2 . in? . (list->vector '(1 2 3))))
  (check-false (4 . in? . (list->vector '(1 2 3))))
  (check-true ('a . in? . (make-hash '((a . 1) (b . 2) (c  . 3)))))
  (check-false ('x . in? . (make-hash '((a . 1) (b . 2) (c  . 3)))))
  (check-true ("o" . in? . "foobar"))
  (check-false ("z" . in? . "foobar"))
  (check-true ('o . in? . 'foobar))
  (check-false ('z . in? . 'foobar))
  (check-true ("F" . in? . #\F))
  
  (check-true (in? "foo" (string->path "/root/foo/bar/file.txt")))
  (check-false (in? "zam" (string->path "/root/foo/bar/file.txt"))))
