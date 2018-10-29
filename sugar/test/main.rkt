#lang racket
(require (for-syntax racket/syntax
                     syntax/strip-context))

(define-syntax (eval-with-and-without-contracts stx)
  (syntax-case stx ()
    [(_ EXPRS ...)
     (with-syntax ([MODULE-WITHOUT-CONTRACTS (generate-temporary)]
                   [MODULE-WITH-CONTRACTS (generate-temporary)])
       (replace-context stx
                        #'(begin
                            (module MODULE-WITHOUT-CONTRACTS racket
                              (require rackunit "../main.rkt" net/url)
                              EXPRS ...)
                            (require 'MODULE-WITHOUT-CONTRACTS)
                            (module MODULE-WITH-CONTRACTS racket
                              (require rackunit (submod "../main.rkt" safe) net/url)
                              EXPRS ...)
                            (require 'MODULE-WITH-CONTRACTS))))]))

(eval-with-and-without-contracts
 (check-equal? (->int 42) 42)
 (check-equal? (->int 42.1) 42)
 (check-equal? (->int 42+3i) 42)
 (check-equal? (->int "42") 42)
 (check-equal? (->int '42) 42)
 (check-equal? (->int (string->path "42")) 42)
 (check-equal? (->int #\A) 65)
 (check-equal? (->int (make-list 42 null)) 42)
 
 (check-equal? (->string "foo") "foo")
 (check-equal? (->string #"foo") "foo")
 (check-equal? (->string '()) "")
 (check-equal? (->string (void)) "")
 (check-equal? (->string 'foo) "foo")
 (check-equal? (->string 123) "123")
 (check-equal? (->string (string->url "foo/bar.html")) "foo/bar.html")
 (define file-name-as-text "foo.txt")
 (check-equal? (->string (string->path file-name-as-text)) file-name-as-text)
 (check-equal? (->string #\¶) "¶")
 
 (check-equal? (->path "foo") (string->path "foo"))
 (check-equal? (->path #"foo") (string->path "foo"))
 (check-equal? (->path 'foo) (string->path "foo"))
 (check-equal? (->path 123) (string->path "123"))
 (check-equal? (->path (string->url "foo/bar.html")) (string->path "foo/bar.html"))
 (check-equal? (->path (string->url "/foo/bar.html")) (string->path "/foo/bar.html"))
 
 (check-equal? (->list '(1 2 3)) '(1 2 3))
 (check-equal? (->list (list->vector '(1 2 3))) '(1 2 3))
 (check-not-false (andmap (lambda (e) (member e '(1 2 3))) (->list (set 1 2 3))))
 (check-equal? (->list "foo") (list "foo"))
 
 (check-true (->boolean #t))
 (check-false (->boolean #f))
 (check-true (->boolean "#f"))
 (check-true (->boolean "foo"))
 (check-true (->boolean '()))
 (check-true (->boolean '(1 2 3)))
 
 
 (module dp racket/base
   (require "../define.rkt")
   (define+provide (dp-f x #:y [y 42] . zs)
     (apply + x y zs)))
 
 (require 'dp)
 (check-equal? (dp-f 1 #:y 0 2 3) 6)
 
 (module dps racket/base
   (require sugar/define)
   (define+provide+safe (dps-f x #:y [y 42] . zs)
     ((integer?) (#:y integer?) #:rest (listof integer?) . ->* . integer?)
     (apply + x y zs)))
 
 (require 'dps)
 (check-equal? (dps-f 1 #:y 0 2 3) 6)
 (require (prefix-in safe: (submod 'dps safe)))
 (check-equal? (safe:dps-f 1 #:y 0 2 3) 6)
 (check-exn exn:fail? (λ _ (safe:dps-f 'foo)))
 
 (module dpsb racket/base
   (require sugar/define)
   (define+provide+safe dpsb-f 
     ((integer?) (#:y integer?) #:rest (listof integer?) . ->* . integer?)
     (λ(x #:y [y 42] . zs) (apply + x y zs))))
 
 (require 'dpsb)
 (check-equal? (dpsb-f 1 #:y 0 2 3) 6)
 (require (prefix-in safe: (submod 'dpsb safe)))
 (check-equal? (safe:dpsb-f 1 #:y 0 2 3) 6)
 (check-exn exn:fail? (λ _ (safe:dpsb-f 'foo)))
 
 (module ps racket/base
   (require "../define.rkt")
   (provide+safe [ps-f ((integer?) (#:y integer?) #:rest (listof integer?) . ->* . integer?)])
   (define (ps-f x #:y [y 42] . zs)
     (apply + x y zs)))
 
 (require 'ps)
 (check-equal? (ps-f 1 #:y 0 2 3) 6)
 (require (prefix-in safe: (submod 'ps safe)))
 (check-equal? (safe:ps-f 1 #:y 0 2 3) 6)
 (check-exn exn:fail? (λ _ (safe:ps-f 'foo)))
 
 (module dcp racket/base
   (require "../define.rkt" rackunit)
   (define/contract+provide (dcp-f x #:y [y 42] . zs)
     ((integer?) (#:y integer?) #:rest (listof integer?) . ->* . integer?)
     (apply + x y zs))
   (check-exn exn:fail? (λ _ (dcp-f 'foo))))
 
 (require 'dcp)
 (check-equal? (dcp-f 1 #:y 0 2 3) 6)
 (check-exn exn:fail? (λ _ (dcp-f 'foo)))
 
 (module dpc racket/base
   (require "../define.rkt" rackunit)
   (define+provide/contract (dpc-f x #:y [y 42] . zs)
     ((integer?) (#:y integer?) #:rest (listof integer?) . ->* . list?)
     (list* x y zs))
   (check-equal? (dpc-f 'foo) '(foo 42))) ; locally, no contract triggered
 
 (require 'dpc)
 (check-equal? (dpc-f 1) '(1 42))
 (check-exn exn:fail? (λ _ (dpc-f 'foo)))
 
 (check-true (members-unique? '(a b c)))
 (check-false (members-unique? '(a b c c)))
 (check-true (members-unique? "zoey"))
 (check-false (members-unique? "zooey"))
 
 (check-equal? (trimf (list 4 1 2 3 4) even?) '(1 2 3))
 (check-equal? (trimf (list 1 3 2 4 5 6 8 9 13) odd?) '(2 4 5 6 8))
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
 (check-exn exn:fail:contract? (λ () (has-ext? #f "foo")))
 (check-exn exn:fail:contract? (λ () (has-ext? "foo" #f)))
 
 (check-equal? (get-ext (->path "foo.txt")) "txt")
 (check-false (get-ext "foo"))
 (check-false (get-ext ".foo"))
 (check-exn exn:fail:contract? (λ () (get-ext #f)))
 
 (check-equal? (add-ext (string->path "foo") "txt") (string->path "foo.txt"))
 (check-exn exn:fail:contract? (λ () (add-ext "foo" #f)))
 (check-exn exn:fail:contract? (λ () (add-ext #f "foo" )))
 
 (check-equal? (remove-ext foo-path) foo-path)
 (check-equal? (remove-ext (->path ".foo.txt")) (->path ".foo"))
 (check-equal? (remove-ext foo.txt-path) foo-path)
 (check-equal? (remove-ext foo.bar.txt-path) foo.bar-path)
 (check-not-equal? (remove-ext foo.bar.txt-path) foo-path) ; does not remove all extensions
 ;; test remove-ext on paths that have "." prefix
 (check-equal? (remove-ext (->path "./foo.txt.bar")) (->path "./foo.txt"))
 (check-equal? (remove-ext (->path "../foo.txt.bar")) (->path "../foo.txt"))
 (check-equal? (remove-ext (->path "/hidden/file/.foo.txt.bar")) (->path "/hidden/file/.foo.txt"))
 
 (check-equal? (remove-ext* foo-path) foo-path)
 (check-equal? (remove-ext* foo.txt-path) foo-path)
 (check-equal? (remove-ext* (->path ".foo.txt")) (->path ".foo"))
 (check-not-equal? (remove-ext* foo.bar.txt-path) foo.bar-path) ; removes more than one ext
 (check-equal? (remove-ext* foo.bar.txt-path) foo-path)
 ;; test remove-ext* on paths that have  "." prefix
 (check-equal? (remove-ext* (->path "./foo.txt.bar")) (->path "./foo"))
 (check-equal? (remove-ext* (->path "../foo.txt.bar")) (->path "../foo"))
 (check-equal? (remove-ext* (->path "/hidden/file/.foo.txt.bar")) (->path "/hidden/file/.foo"))
 
 (check-true (has-binary-ext? "foo.MP3"))
 (check-false (has-binary-ext? "foo.py"))
 
 (check-equal? (slice-at (range 5) 1) '((0) (1) (2) (3) (4)))
 (check-equal? (slice-at (range 5) 2) '((0 1) (2 3) (4)))
 (check-equal? (slice-at (range 5) 2 #t) '((0 1) (2 3)))
 (check-equal? (slice-at (range 5) 3) '((0 1 2) (3 4)))
 (check-equal? (slice-at (range 5) 3 #t) '((0 1 2)))
 
 (check-equal? (slicef '(0 1 2 0 0 0 3) positive?) '((0) (1 2) (0 0 0) (3)))
 (check-equal? (slicef '(0 1 2 0 0 0 3) positive?) (slicef '(0 1 2 0 0 0 3) zero?))
 (check-equal? (slicef '(1 (1) (1) 1 1 1 (1)) list?) '((1) ((1) (1)) (1 1 1) ((1))))
 (check-equal? (slicef '(1 2 3 4 5) list?) '((1 2 3 4 5)))
 
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
 (check-equal? (map (λ(a b c) (list a b c)) (shift xs -1 'ignored #t) (shift xs 0 'ignored #t) (shift xs 1 'ignored #t)) '((1 0 4) (2 1 0) (3 2 1) (4 3 2) (0 4 3)))
 (check-equal? (shifts xs '(-1 0 1) 'boing)  `((1 2 3 4 boing) ,xs (boing 0 1 2 3)))
 (check-equal? (shifts xs '(-1 0 1) 'boing #t)  `((1 2 3 4 0) ,xs (4 0 1 2 3)))
 (check-equal? (shift xs 5 0) (make-list 5 0))
 (check-exn exn:fail? (λ() (shift xs -10)))

 (check-equal? (map (λ(a b c) (list a b c)) (shift-left xs -1) (shift-left xs 0) (shift-left xs 1)) (map reverse '((1 0 #f) (2 1 0) (3 2 1) (4 3 2) (#f 4 3))))

 (check-equal? (shift-cycle xs 2) '(3 4 0 1 2))
 (check-equal? (shift-left-cycle xs 2) '(2 3 4 0 1))
 (check-equal? (shift-cycle xs 7) '(3 4 0 1 2))
 (check-equal? (shift-left-cycle xs 7) '(2 3 4 0 1))
 (check-equal? (shift-cycle xs 107) '(3 4 0 1 2))
 (check-equal? (shift-left-cycle xs 107) '(2 3 4 0 1))
 
 (check-true (urlish? (->path "/Users/MB/home.html")))
 (check-true (urlish? "/Users/MB/home.html?foo=bar"))
 (check-true (urlish? (->symbol "/Users/MB/home")))
 
 (check-true (pathish? (->path "/Users/MB/home")))
 (check-true (pathish? "/Users/MB/home"))
 (check-true (pathish? (->symbol "/Users/MB/home")))
 
 (check-equal? (filter-split '("foo" " " "bar" "\n" "\n" "ino") (λ(x) (< (string-length x) 3))) '(("foo")("bar")("ino")))
 
 (check-exn exn:fail? (λ _ (slice-at (range 5) 0))) ; needs a positive integer as second arg
 (check-exn exn:fail? (λ _ (slicef-at (range 5) 3))) ; needs a procedure as second arg
 
 
 (define ys (range 5))
 (check-equal? (values->list (shift/values ys -1 'boing)) '(1 2 3 4 boing))
 (check-equal? (values->list (shift/values ys '(-1 0 1) 'boing)) `((1 2 3 4 boing) ,xs (boing 0 1 2 3)))

 (check-equal? "42 = 42\n" (let ([os (open-output-string)])
                             (parameterize ([current-error-port os])
                               (report 42))
                             (get-output-string os)))

 (check-equal? "(quotient/remainder 10 3) = (values 3 1)\n" (let ([os (open-output-string)])
                                                              (parameterize ([current-error-port os])
                                                                (report (quotient/remainder 10 3))
                                                                (get-output-string os)))))

