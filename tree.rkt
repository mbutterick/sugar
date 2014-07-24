#lang racket/base
(require "define.rkt")

(module+ test (require rackunit))

(define+provide/contract (filter-tree proc tree)
  (procedure? list? . -> . list?)
  (define (remove-empty x)
    (cond
      [(list? x) (filter (compose1 not null?) (map remove-empty x))]
      [else x]))
  
  (define (filter-tree-inner proc x)
    (cond
      [(list? x) (map (λ(i) (filter-tree-inner proc i)) x)]
      [else (if (proc x) x null)]))
  
  (remove-empty (filter-tree-inner proc tree)))
 

(define+provide/contract (filter-not-tree proc tree)
  (procedure? list? . -> . list?)
  (filter-tree (λ(i) (not (proc i))) tree))


(define+provide/contract (map-tree proc tree)
  (procedure? list? . -> . list?)
  (cond 
    [(list? tree) (map (λ(i) (map-tree proc i)) tree)]
    [else (proc tree)]))

