#lang racket/base
(require racket/list racket/contract)
(require "define.rkt")

(module+ test (require rackunit))

;; apply filter proc recursively
(define+provide/contract (filter-tree proc tree)
  (procedure? list? . -> . list?)
  (define (remove-empty x)
    (cond
      [(list? x) (filter-not empty? (map remove-empty x))]
      [else x]))
  
  (define (filter-tree-inner proc x)
    (cond
      [(list? x) (map (λ(i) (filter-tree-inner proc i)) x)]
      [else (if (proc x) x empty)]))
  
  (remove-empty (filter-tree-inner proc tree)))


(module+ test
  (check-equal? (filter-tree string? '(p)) empty)
  (check-equal? (filter-tree string? '(p "foo" "bar")) '("foo" "bar"))
  (check-equal? (filter-tree string? '(p "foo" (p "bar"))) '("foo" ("bar")))
  (check-equal? (filter-tree (λ(i) (and (string? i) (equal? i "\n"))) '("\n" (foo "bar") "\n")) '("\n" "\n"))) 

;; apply filter-not proc recursively
(define+provide/contract (filter-not-tree proc tree)
  (procedure? list? . -> . list?)
  (filter-tree (λ(i) (not (proc i))) tree))

(module+ test
  (check-equal? (filter-not-tree string? '(p)) '(p))
  (check-equal? (filter-not-tree string? '(p "foo" "bar")) '(p))
  (check-equal? (filter-not-tree string? '(p "foo" (p "bar"))) '(p (p)))
  ;(check-equal? (filter-tree (λ(i) (and (tagged-xexpr? i) (equal? 'em (car i)))) '(p "foo" (em "bar"))) '(p "foo"))
  )


;; todo: doc this function
(define+provide/contract (map-tree proc tree)
  (procedure? list? . -> . list?)
  (cond 
    [(list? tree) (map (λ(i) (map-tree proc i)) tree)]
    [else (proc tree)]))

(module+ test
  (check-equal? (map-tree (λ(i) (if (number? i) (* 2 i) i)) '(p 1 2 3 (em 4 5))) '(p 2 4 6 (em 8 10)))
  (check-equal? (map-tree (λ(i) (if (symbol? i) 'foo i)) '(p 1 2 3 (em 4 5))) '(foo 1 2 3 (foo 4 5))))