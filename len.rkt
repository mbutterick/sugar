#lang racket/base
(require racket/set)

(provide len)

(define (len x)
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(symbol? x) (len (symbol->string x))]
    [(path? x) (len (path->string x))]
    [(vector? x) (vector-length x)]
    [(hash? x) (len (hash-keys x))]
    [(integer? x) (len (number->string x))]
    [(set? x) (len (set->list x))]
    [else (error "len: can’t calculate length of" x)]))