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
(check-exn exn:fail? (λ() (get '(0 1 2 3 4 5) 100))) ; index too big
(check-equal? (get `(0 1 ,(list 2) 3 4 5) 2) (list 2))
(check-equal? (get '(0 1 2 3 4 5) 0 2) '(0 1))
(check-equal? (get (list->vector '(0 1 2 3 4 5)) 2) 2)
(check-equal? (get (list->vector'(0 1 2 3 4 5)) 0 2) (list->vector '(0 1)))
(check-equal? (get "purple" 2) "r")
(check-equal? (get "purple" 0 2) "pu")
(check-equal? (get 'purple 2) 'r)
(check-equal? (get 'purple 0 2) 'pu)
(check-equal? (get (string->path "/root/foo/bar/file.txt") 2) (string->path "foo"))
(check-equal? (get (string->path "/root/foo/bar/file.txt") 0 2) (list (string->path "/") (string->path "root")))
(check-equal? (get (make-hash `((a . ,(list 1)) (b . ,(list 2)) (c  . ,(list 3)))) 'a) (list 1))
(check-exn exn:fail? (λ() (get (make-hash `((a . ,(list 1)) (b . ,(list 2)) (c  . ,(list 3)))) 'z))) ; nonexistent key

(check-equal? (get (string->path "/root/foo/bar/file.txt") 1) (string->path "root"))
(check-equal? (get (string->path "/root/foo/bar/file.txt") 0 3)
              (map string->path '("/" "root" "foo")))


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
(check-true ("F" . in? . #\F))

(check-true (in? "foo" (string->path "/root/foo/bar/file.txt")))
(check-false (in? "zam" (string->path "/root/foo/bar/file.txt")))


(check-true ("foobar" . starts-with? . "foo"))
(check-true ("foobar" . starts-with? . "f"))
(check-true ("foobar" . starts-with? . "foobar"))
(check-false ("foobar" . starts-with? . "bar"))
(check-false ("foobar" . starts-with? . "."))
(check-true ("foobar" . ends-with? . "bar"))
(check-true ("foobar" . ends-with? . "r"))
(check-true ("foobar" . ends-with? . "foobar"))
(check-false ("foobar" . ends-with? . "foo"))

;  (check-equal? (trim (list "\n" " " 1 2 3 "\n") whitespace?) '(1 2 3))
(check-equal? (trimf (list 1 3 2 4 5 6 8 9 13) odd?) '(2 4 5 6 8))
;(check-equal? (filter-split '("foo" " " "bar" "\n" "\n" "ino") whitespace?) '(("foo")("bar")("ino")))
(check-equal? (filter-split '(1 2 3 4 5 6) even?) '((1)(3)(5)))


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


(define foo-path-strings '("foo" "foo.txt" "foo.bar" "foo.bar.txt"))
(define-values (foo-path foo.txt-path foo.bar-path foo.bar.txt-path) 
  (apply values (map ->path foo-path-strings)))
;; test the sample paths before using them for other tests
(define foo-paths (list foo-path foo.txt-path foo.bar-path foo.bar.txt-path))
(for-each check-equal? (map ->string foo-paths) foo-path-strings)


(check-false (has-ext? foo-path 'txt)) 
(check-true (foo.txt-path . has-ext? . 'txt))
(check-true ((->path "foo.TXT") . has-ext? . 'txt))
(check-true (has-ext? foo.bar.txt-path 'txt))
(check-false (foo.bar.txt-path . has-ext? . 'doc)) ; wrong extension


(check-equal? (get-ext (->path "foo.txt")) "txt")
(check-false (get-ext "foo"))

(check-equal? (add-ext (string->path "foo") "txt") (string->path "foo.txt"))
(check-equal? (remove-ext foo-path) foo-path)
(check-equal? (remove-ext (->path ".foo.txt")) (->path ".foo.txt"))
(check-equal? (remove-ext foo.txt-path) foo-path)
(check-equal? (remove-ext foo.bar.txt-path) foo.bar-path)
(check-not-equal? (remove-ext foo.bar.txt-path) foo-path) ; does not remove all extensions


(check-equal? (remove-ext* foo-path) foo-path)
(check-equal? (remove-ext* foo.txt-path) foo-path)
(check-equal? (remove-ext* (->path ".foo.txt")) (->path ".foo.txt"))
(check-not-equal? (remove-ext* foo.bar.txt-path) foo.bar-path) ; removes more than one ext
(check-equal? (remove-ext* foo.bar.txt-path) foo-path)


(check-true (starts-with? "foobar" "foo"))
(check-true (starts-with? "foobar" "foobar"))
(check-false (starts-with? "foobar" "zam"))
(check-false (starts-with? "foobar" "foobars"))
(check-true (ends-with? "foobar" "bar"))
(check-false (ends-with? "foobar" "zam"))
(check-true (ends-with? "foobar" "foobar"))
(check-false (ends-with? "foobar" "foobars"))
(check-true (capitalized? "Brennan"))
(check-false (capitalized? "foobar"))