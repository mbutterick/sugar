#lang racket/base

(require rackunit net/url racket/set)
(require "main.rkt")

(check-equal? (->string "foo") "foo")
(check-equal? (->string '()) "")
(check-equal? (->string 'foo) "foo")
(check-equal? (->string 123) "123")
;(check-equal? (->string (string->url "foo/bar.html")) "foo/bar.html")
(define file-name-as-text "foo.txt")
(check-equal? (->string (string->path file-name-as-text)) file-name-as-text)
(check-equal? (->string #\¶) "¶")



(check-equal? (->path "foo") (string->path "foo"))
(check-equal? (->path 'foo) (string->path "foo"))
(check-equal? (->path 123) (string->path "123"))
(check-equal? (->path (string->url "foo/bar.html")) (string->path "foo/bar.html"))

(check-equal? (->list '(1 2 3)) '(1 2 3))
(check-equal? (->list (list->vector '(1 2 3))) '(1 2 3))
(check-equal? (->list (set 1 2 3)) '(3 2 1))
(check-equal? (->list "foo") (list "foo"))

(check-true (->boolean #t))
(check-false (->boolean #f))
(check-true (->boolean "#f")) 
(check-true (->boolean "foo"))
(check-true (->boolean '()))
(check-true (->boolean '(1 2 3)))


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
(check-not-equal? (len (make-hash '((a . 1) (b . 2)))) 3) ; len 2


(check-equal? (get '(0 1 2 3 4 5) 2) 2)
(check-equal? (get `(0 1 ,(list 2) 3 4 5) 2) (list 2))
(check-equal? (get '(0 1 2 3 4 5) 0 2) '(0 1))
(check-equal? (get '(0 1 2 3 4 5) 2 -1) '(2 3 4))
(check-equal? (get '(0 1 2 3 4 5) 2 'end) '(2 3 4 5))
(check-equal? (get (list->vector '(0 1 2 3 4 5)) 2) 2)
(check-equal? (get (list->vector'(0 1 2 3 4 5)) 0 2) (list->vector '(0 1)))
(check-equal? (get (list->vector'(0 1 2 3 4 5)) 2 -1) (list->vector '(2 3 4)))
(check-equal? (get (list->vector'(0 1 2 3 4 5)) 2 'end) (list->vector '(2 3 4 5)))
(check-equal? (get "purple" 2) "r")
(check-equal? (get "purple" 0 2) "pu")
(check-equal? (get "purple" 2 -1) "rpl")
(check-equal? (get "purple" 2 'end) "rple")
(check-equal? (get 'purple 2) 'r)
(check-equal? (get 'purple 0 2) 'pu)
(check-equal? (get 'purple 2 -1) 'rpl)
(check-equal? (get 'purple 2 'end) 'rple)
(check-equal? (get (make-hash `((a . ,(list 1)) (b . ,(list 2)) (c  . ,(list 3)))) 'a) (list 1))


(check-true (2 . in? . '(1 2 3)))
(check-false (4 . in? . '(1 2 3)))
(check-true (2 . in? . (list->vector '(1 2 3))))
(check-false (4 . in? . (list->vector '(1 2 3))))
(check-true ('a . in? . (make-hash '((a . 1) (b . 2) (c  . 3)))))
(check-false ('x . in? . (make-hash '((a . 1) (b . 2) (c  . 3)))))
(check-true ("o" . in? . "foobar"))
(check-false ("z" . in? . "foobar"))
(check-true ('o . in? . 'foobar))
(check-false ('z . in? . 'foobar))
(check-false ("F" . in? . #\F))


(check-true ("foobar" . starts-with? . "foo"))
(check-true ("foobar" . starts-with? . "f"))
(check-true ("foobar" . starts-with? . "foobar"))
(check-false ("foobar" . starts-with? . "bar"))
(check-true ("foobar" . ends-with? . "bar"))
(check-true ("foobar" . ends-with? . "r"))
(check-true ("foobar" . ends-with? . "foobar"))
(check-false ("foobar" . ends-with? . "foo"))

;  (check-equal? (trim (list "\n" " " 1 2 3 "\n") whitespace?) '(1 2 3))
(check-equal? (trim (list 1 3 2 4 5 6 8 9 13) odd?) '(2 4 5 6 8))
;(check-equal? (splitf-at* '("foo" " " "bar" "\n" "\n" "ino") whitespace?) '(("foo")("bar")("ino")))
(check-equal? (splitf-at* '(1 2 3 4 5 6) even?) '((1)(3)(5)))


(check-equal? (filter-tree string? '(p)) null)
(check-equal? (filter-tree string? '(p "foo" "bar")) '("foo" "bar"))
(check-equal? (filter-tree string? '(p "foo" (p "bar"))) '("foo" ("bar")))
(check-equal? (filter-tree (λ(i) (and (string? i) (equal? i "\n"))) '("\n" (foo "bar") "\n")) '("\n" "\n"))
(check-equal? (filter-not-tree string? '(p)) '(p))
(check-equal? (filter-not-tree string? '(p "foo" "bar")) '(p))
(check-equal? (filter-not-tree string? '(p "foo" (p "bar"))) '(p (p)))
;(check-equal? (filter-tree (λ(i) (and (tagged-xexpr? i) (equal? 'em (car i)))) '(p "foo" (em "bar"))) '(p "foo"))

(check-equal? (map-tree (λ(i) (if (number? i) (* 2 i) i)) '(p 1 2 3 (em 4 5))) '(p 2 4 6 (em 8 10)))
(check-equal? (map-tree (λ(i) (if (symbol? i) 'foo i)) '(p 1 2 3 (em 4 5))) '(foo 1 2 3 (foo 4 5)))