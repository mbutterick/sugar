#lang racket/base
(require "define.rkt" "coerce/base.rkt")


;; this is identical to `filename-extension` in `racket/path`
;; but will not treat hidden files as an extension (which IMHO is a bug)
(define (filename-extension name)
  (let* ([name (file-name-from-path name)]
         [name (and name (path->bytes name))])
    (cond [(and name (regexp-match #rx#".[.]([^.]+)$" name)) => cadr]
          [else #f])))

(module+ test
  (require rackunit)
  (require (prefix-in rp: racket/path))
  (check-equal? (rp:filename-extension (string->path ".foo")) #"foo") ; bad behavior
  (check-false (filename-extension (string->path ".foo")))) ; good behavior


;; this is pulled in from `racket/path` to avoid the dependency
(define (file-name-from-path name)
  (unless (or (path-string? name)
              (path-for-some-system? name))
    (raise-argument-error 'file-name-from-path "(or/c path-string? path-for-some-system?)" name))
  (let-values ([(base file dir?) (split-path name)])
    (and (not dir?) (path-for-some-system? file) file)))


;; does path have a certain extension, case-insensitively
(define+provide+safe (has-ext? x ext)
  (pathish? stringish? . -> . boolean?)
  (define ext-of-path (filename-extension (->path x)))
  (and ext-of-path (equal? (string-downcase (bytes->string/utf-8 ext-of-path)) (string-downcase (->string ext)))))


;; get file extension as a string, or return #f 
;; (consistent with filename-extension behavior)
(define+provide+safe (get-ext x)
  (pathish? . -> . (or/c #f string?))
  (let ([fe-result (filename-extension (->path x))])
    (and fe-result (bytes->string/utf-8 fe-result))))


;; todo: add extensions
(provide+safe binary-extensions)
(define binary-extensions
  (map symbol->string '(gif jpg jpeg mp3 png zip pdf ico tar ai eps exe)))


(define+provide+safe (has-binary-ext? x)
  (pathish? . -> . boolean?)
  (let ([x (->path x)])
    (and (ormap (λ(ext) (has-ext? x ext)) binary-extensions) #t)))


;; put extension on path
;; use local contract here because this function is used within module
(define+provide+safe (add-ext x ext)
  (stringish? stringish? . -> . pathish?)
  (->path (string-append (->string x) "." (->string ext))))


(define (starts-with? str starter)
  (define pat (regexp (format "^~a" (regexp-quote starter))))
  (and (regexp-match pat str) #t))

(define (path-hidden? path)
  ((->string (file-name-from-path path)) . starts-with? . "."))

;; with `remove-ext` and `remove-ext*`,
;; the policy is to pass through hidden files
;; though I can't remember why.

;; take one extension off path
(define+provide+safe (remove-ext x)
  (pathish? . -> . path?)
  (let ([path (->path x)])
    (if (path-hidden? path)
        path
        (path-replace-suffix path ""))))


;; take all extensions off path
(define+provide+safe (remove-ext* x)
  (pathish? . -> . path?)
  (let loop ([path (->path x)])
    (define path-out (remove-ext path))
    (if (equal? path path-out)
        path
        (loop path-out))))