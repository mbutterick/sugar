#lang racket/base
(require "../define.rkt" "../coerce.rkt")


(define+provide+safe (starts-with? str starter)
  (string? string? . -> . coerce/boolean?)
  (define pat (regexp (format "^~a" (regexp-quote starter))))
  (and (regexp-match pat (->string str)) #t))


(define+provide+safe (ends-with? str ender)
  (string? string? . -> . coerce/boolean?)
  (define pat (regexp (format "~a$" (regexp-quote ender))))
  (and (regexp-match pat (->string str)) #t))


(define+provide+safe (capitalized? str-in)
  (string? . -> . coerce/boolean?)
  (define str (->string str-in))
  (and (positive? (string-length str))
       (char-upper-case? (car (string->list (car (regexp-match "." str)))))))


(module+ test
  (require rackunit)  
  (check-true (starts-with? "foobar" "foo"))
  (check-true (starts-with? "foobar" "foobar"))
  (check-false (starts-with? "foobar" "zam"))
  (check-false (starts-with? "foobar" "foobars"))
  (check-false (starts-with? "foo" "."))
  (check-true (ends-with? "foobar" "bar"))
  (check-false (ends-with? "foobar" "zam"))
  (check-true (ends-with? "foobar" "foobar"))
  (check-false (ends-with? "foobar" "foobars"))
  (check-true (capitalized? "Brennan"))
  (check-false (capitalized? "foobar")))

