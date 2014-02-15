#lang racket/base
(require racket/contract racket/set)

(provide len)

(define/contract (has-length? x)
  (any/c . -> . boolean?)
  (ormap (λ(proc) (proc x)) (list list? string? symbol? vector? hash? set?)))

;; general way of asking for length
(define/contract (len x)
  (any/c . -> . (or/c integer? #f))
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(symbol? x) (len (symbol->string x))]
    [(vector? x) (len (vector->list x))]
    [(hash? x) (len (hash-keys x))]
    [(set? x) (len (set->list x))]
    [(integer? x) (len (number->string x))]
    [else (error "len: can’t calculate length of" x)]))