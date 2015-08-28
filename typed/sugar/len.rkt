#lang typed/racket/base
(require racket/set racket/sequence)
(require typed/sugar/define)

(provide Lengthable)
(define-type Lengthable (U (Listof Any) String Symbol Path (Vectorof Any) HashTableTop (Setof Any)))

(define/typed+provide (len x)
  (Lengthable -> Nonnegative-Integer)
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