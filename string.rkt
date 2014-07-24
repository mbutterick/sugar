#lang racket/base
(require "define.rkt" "coerce.rkt")


(define+provide/contract (starts-with? str starter)
  (coerce/string? coerce/string? . -> . coerce/boolean?)
  (and (<= (string-length starter) (string-length str)) 
       (equal? (substring str 0 (string-length starter)) starter)))


(define+provide/contract (ends-with? str ender)
  (coerce/string? coerce/string? . -> . coerce/boolean?)
  (and (<= (string-length ender) (string-length str)) 
       (equal? (substring str (- (string-length str) (string-length ender)) (string-length str)) ender)))

(define+provide/contract (capitalized? str)
  (coerce/string? . -> . coerce/boolean?)
  (char-upper-case? (car (string->list str))))
