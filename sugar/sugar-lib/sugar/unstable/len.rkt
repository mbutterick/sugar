#lang racket/base
(require (for-syntax
          racket/base
          racket/syntax)
         "../define.rkt"
         racket/sequence
         racket/generic)


(provide define-generics+provide+safe)
(define-syntax (define-generics+provide+safe stx)
  (syntax-case stx ()
    [(_ TYPE ID-CONTRACT (ID . ID-ARGS) . ARGS)
     (with-syntax ([TYPE? (format-id stx "~a?" #'TYPE)])
       #'(begin
           (provide TYPE? ID)
           (module+ safe
             (require racket/contract)
             (provide TYPE? (contract-out [ID ID-CONTRACT])))
           (define-generics TYPE (ID . ID-ARGS) . ARGS)))]))

(provide len lengthable?)
(define-generics lengthable
  (len lengthable)
  #:fast-defaults
  ([list? (define len length)]
    [string? (define len string-length)]
    [symbol? (define len (compose1 string-length symbol->string))]
    [path? (define len (compose1 string-length path->string))]
    [vector? (define len vector-length)]
    [hash? (define (len x) (length (hash-keys x)))]
    [(λ (x) (and (sequence? x) (not (integer? x)))) (define len (compose1 length sequence->list))]))


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