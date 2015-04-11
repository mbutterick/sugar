#lang typed/racket/base
(require typed/sugar/define "coerce.rkt")

(define/typed+provide (starts-with? str starter)
  (Stringish Stringish -> Boolean)
  (let ([str (->string str)]
        [starter (->string starter)])
    (and (<= (string-length starter) (string-length str)) 
         (equal? (substring str 0 (string-length starter)) starter))))

(define/typed+provide (ends-with? str ender)
  (Stringish Stringish -> Boolean)
  (let ([str (->string str)]
        [ender (->string ender)])
    (and (<= (string-length ender) (string-length str)) 
         (equal? (substring str (- (string-length str) (string-length ender)) (string-length str)) ender))))

(define/typed+provide (capitalized? str)
  (Stringish -> Boolean)
  (let ([str (->string str)])
    (char-upper-case? (car (string->list str)))))
