#lang racket/base
(require "define.rkt" racket/set racket/sequence)

(define+provide+safe (len x)
  ((or/c list? vector? set? sequence? string? symbol? path? hash?) . -> . integer?)
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(symbol? x) (len (symbol->string x))]
    [(path? x) (len (path->string x))]
    [(vector? x) (vector-length x)]
    [(hash? x) (len (hash-keys x))]
    [(set? x) (len (set->list x))]
    [(and (sequence? x) (not (integer? x))) (len (sequence->list x))]
    [else (error "len: can't calculate length of" x)]))