#lang racket/base
(require "define/contract.rkt" "coercion.rkt")


(define+provide/contract (starts-with? str starter)
  (string? string? . -> . coerce/boolean?)
  (define starter-pattern (regexp (format "^~a" starter)))
  (regexp-match starter-pattern str))


(define+provide/contract (ends-with? str ender)
  (string? string? . -> . coerce/boolean?)
  (define ender-pattern (regexp (format "~a$" ender)))
  (regexp-match ender-pattern str))