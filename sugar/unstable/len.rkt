#lang racket/base
(require "../define.rkt" racket/sequence)

(define+provide+safe (len x)
  ((or/c list? vector? sequence? string? symbol? path? hash?) . -> . integer?)
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(symbol? x) (len (symbol->string x))]
    [(path? x) (len (path->string x))]
    [(vector? x) (vector-length x)]
    [(hash? x) (len (hash-keys x))]
    [(and (sequence? x) (not (integer? x))) (len (sequence->list x))]
    [else (error "len: can't calculate length of" x)]))


(module+ test
  (require rackunit racket/set)
   (check-equal? (len '(1 2 3)) 3)
 (check-not-equal? (len '(1 2)) 3) ; len 2
 (check-equal? (len "foo") 3)
 (check-not-equal? (len "fo") 3) ; len 2
 (check-equal? (len 'foo) 3)
 (check-not-equal? (len 'fo) 3) ; len 2
 (check-equal? (len (list->vector '(1 2 3))) 3)
 (check-not-equal? (len (list->vector '(1 2))) 3) ; len 2
 (check-equal? (len (set 1 2 3)) 3)
 (check-not-equal? (len (set 1 2)) 3) ; len 2
 (check-equal? (len (make-hash '((a . 1) (b . 2) (c . 3)))) 3)
 (check-not-equal? (len (make-hash '((a . 1) (b . 2)))) 3)) ; len 2