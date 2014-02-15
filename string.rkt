#lang racket/base
(require racket/contract)
(require "coerce.rkt" "container.rkt" "len.rkt" "exception.rkt")

(provide starts-with? ends-with? stringish?)

;; stringish: data type that can be trivially converted to string
;; todo: merge this with pathish
(define/contract (stringish? x)
  (any/c . -> . boolean?)
  (try (->boolean (->string x)) (except [exn:fail? (λ(e) #f)])))

;; python-style string testers
(define/contract (starts-with? str starter)
  (stringish? stringish? . -> . boolean?)
  (let ([str (->string str)]
        [starter (->string starter)])
    (and (<= (len starter) (len str)) (equal? (get str 0 (len starter)) starter))))


(define/contract (ends-with? str ender)
  (string? string? . -> . boolean?)
  (and (<= (len ender) (len str)) (equal? (get str (- (len str) (len ender)) 'end) ender)))