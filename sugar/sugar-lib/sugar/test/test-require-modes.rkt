#lang racket/base
(require rackunit)

(module rb racket/base
  (require (submod sugar/list safe) rackunit)
  (provide (all-defined-out))
  (check-exn exn:fail:contract? (λ _ (trimf odd? '(1 2 3)))) ; fails at trimf 
  (define foo (trimf '(1 2 3) odd?))
  (check-equal? foo '(2)))

(module rbu racket/base
  (require sugar/list rackunit)
  (provide (all-defined-out))
  (check-exn exn:fail:contract? (λ _ (trimf odd? '(1 2 3)))) ; fails at dropf
  (define foo (trimf '(1 2 3) odd?))
  (check-equal? foo '(2)))

(require (prefix-in rb: 'rb))
(require (prefix-in rbu: 'rbu))

(check-true (andmap (λ(val) (equal? val '(2))) (list rb:foo rbu:foo)))