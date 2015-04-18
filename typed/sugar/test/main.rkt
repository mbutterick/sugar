#lang typed/racket/base
(require racket/include typed/rackunit typed/net/url racket/set racket/list racket/match)
(require typed/sugar)

;; begin shared typed / untyped tests

(check-equal? (->int 42) 42)
(check-equal? (->int 42.1) 42)
(check-equal? (->int 42+3i) 42)
(check-equal? (->int "42") 42)
(check-equal? (->int '42) 42)
(check-equal? (->int (string->path "42")) 42)
(check-equal? (->int #\A) 65)
(check-equal? (->int (make-list 42 null)) 42)

(check-equal? (->string "foo") "foo")
(check-equal? (->string '()) "")
(check-equal? (->string (void)) "")
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





(check-true ("foobar" . starts-with? . "foo"))
(check-true ("foobar" . starts-with? . "f"))
(check-true ("foobar" . starts-with? . "foobar"))
(check-false ("foobar" . starts-with? . "bar"))
(check-false ("foobar" . starts-with? . "."))
(check-true ("foobar" . ends-with? . "bar"))
(check-true ("foobar" . ends-with? . "r"))
(check-true ("foobar" . ends-with? . "foobar"))
(check-false ("foobar" . ends-with? . "foo"))


(check-true (members-unique? '(a b c)))
(check-false (members-unique? '(a b c c)))
(check-true (members-unique? "zoey"))
(check-false (members-unique? "zooey"))

(check-equal? (trimf (list 4 1 2 3 4) even?) '(1 2 3))
(check-equal? (trimf (list 1 3 2 4 5 6 8 9 13) odd?) '(2 4 5 6 8))
(check-equal? (filter-split '("foo" " " "bar" "\n" "\n" "ino") (λ:([x : String]) (< (string-length x) 3))) '(("foo")("bar")("ino")))
(check-equal? (filter-split '(1 2 3 4 5 6) even?) '((1)(3)(5)))


(define foo-path-strings '("foo" "foo.txt" "foo.bar" "foo.bar.txt"))
(match-define (list foo-path foo.txt-path foo.bar-path foo.bar.txt-path) (map ->path foo-path-strings))
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

(check-equal? (slice-at (range 5) 1) '((0) (1) (2) (3) (4)))
(check-equal? (slice-at (range 5) 2) '((0 1) (2 3) (4)))
(check-equal? (slice-at (range 5) 2 #t) '((0 1) (2 3)))
(check-equal? (slice-at (range 5) 3) '((0 1 2) (3 4)))
(check-equal? (slice-at (range 5) 3 #t) '((0 1 2)))

(check-equal? (slicef-at (range 5) even?) '((0 1) (2 3) (4)))
(check-equal? (slicef-at (range 5) odd?) '((0) (1 2) (3 4)))
(check-equal? (slicef-at (range 5) odd? #t) '((1 2) (3 4)))
(check-equal? (slicef-at (range 5) procedure?) '((0 1 2 3 4)))

(check-equal? (slicef-at '(1 2 2 1 2) even?) '((1) (2) (2 1) (2)))
(check-equal? (slicef-at '(1 2 2 1 2) even? #t) '((2) (2 1) (2)))

(check-equal? (sublist (range 5) 0 0) '())
(check-equal? (sublist (range 5) 0 1) '(0))
(check-equal? (sublist (range 5) 0 5) '(0 1 2 3 4))

(check-equal? (break-at '(5 6 7 8) '()) '((5 6 7 8)))
(check-equal? (break-at '(5 6 7 8) '(0)) '((5 6 7 8)))
(check-equal? (break-at '(5 6 7 8) '(1 2 3)) '((5) (6) (7) (8)))
(check-equal? (break-at '(5 6 7 8) '(1 3)) '((5) (6 7) (8)))
(check-equal? (break-at '(5 6 7 8) '(1)) (break-at '(5 6 7 8) 1))

(define xs (range 5))
(check-equal? (map (λ(a b c) (list a b c)) (shift xs -1) (shift xs 0) (shift xs 1)) '((1 0 #f) (2 1 0) (3 2 1) (4 3 2) (#f 4 3)))
(check-equal? (shift xs '(-1 0 1) 'boing)  `((1 2 3 4 boing) ,xs (boing 0 1 2 3)))
(check-equal? (shift xs 5 0) (make-list 5 0))
(check-exn exn:fail? (λ() (shift xs -10)))

;; end shared tests


#|
;; todo: revise `check-typing-fails` to make it compatible with 6.0
(check-typing-fails (slice-at (range 5) 0)) ; needs a positive integer as second arg
(check-typing-fails (slicef-at (range 5) 3)) ; needs a procedure as second arg
|#