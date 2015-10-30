#lang racket/base
(require "define.rkt" "coerce.rkt")
 

(define+provide+safe (starts-with? str starter)
  (string? string? . -> . coerce/boolean?)
  (let ([str (->string str)]
        [starter (->string starter)])
    (and (<= (string-length starter) (string-length str)) 
         (equal? (substring str 0 (string-length starter)) starter))))


(define+provide+safe (ends-with? str ender)
  (string? string? . -> . coerce/boolean?)
  (let ([str (->string str)]
        [ender (->string ender)])
    (and (<= (string-length ender) (string-length str)) 
         (equal? (substring str (- (string-length str) (string-length ender)) (string-length str)) ender))))


(define+provide+safe (capitalized? str)
  (string? . -> . coerce/boolean?)
  (let ([str (->string str)])
    (char-upper-case? (car (string->list str)))))

