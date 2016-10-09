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
  (unless (pathish? x)
    (raise-argument-error 'has-ext? "pathish?" x))
  (unless (stringish? ext)
    (raise-argument-error 'has-ext? "stringish?" ext))
  (define ext-of-path (filename-extension (->path x)))
  (and ext-of-path (equal? (string-downcase (bytes->string/utf-8 ext-of-path)) (string-downcase (->string ext)))))


;; get file extension as a string, or return #f 
;; (consistent with filename-extension behavior)
(define+provide+safe (get-ext x)
  (pathish? . -> . (or/c #f string?))
  (unless (pathish? x)
    (raise-argument-error 'get-ext "pathish?" x))
  (let ([fe-result (filename-extension (->path x))])
    (and fe-result (bytes->string/utf-8 fe-result))))


;; todo: add extensions
(provide+safe binary-extensions)
(define binary-extensions
  (map symbol->string '(gif jpg jpeg mp3 png zip pdf ico tar ai eps exe)))


(define+provide+safe (has-binary-ext? x)
  (pathish? . -> . boolean?)
  (unless (pathish? x)
    (raise-argument-error 'has-binary-ext? "pathish?" x))
  (let ([x (->path x)])
    (and (ormap (λ(ext) (has-ext? x ext)) binary-extensions) #t)))


;; put extension on path
;; use local contract here because this function is used within module
(define+provide+safe (add-ext x ext)
  (stringish? stringish? . -> . pathish?)
  (unless (stringish? x)
    (raise-argument-error 'add-ext "stringish?" x))
  (unless (stringish? ext)
    (raise-argument-error 'add-ext "stringish?" ext))
  (->path (string-append (->string x) "." (->string ext))))


(define (starts-with? str starter)
  (define pat (regexp (format "^~a" (regexp-quote starter))))
  (and (regexp-match pat str) #t))


(define (path-hidden? path)
  ((->string (file-name-from-path path)) . starts-with? . "."))


(define (do what path)
  (define reversed-path-elements (reverse (explode-path path)))
  (apply build-path `(,@(reverse (cdr reversed-path-elements))
                      ,(if (eq? what 'hide)
                           (format ".~a" (->string (car reversed-path-elements)))
                           (regexp-replace #rx"^." (->string (car reversed-path-elements)) "")))))


;; take one extension off path
(define+provide+safe (remove-ext x)
  (pathish? . -> . path?)
  (let ([path (->path x)])
    ;; `path-replace-suffix` incorrectly thinks any leading dot counts as a file extension
    ;; when it might be a hidden path.
    ;; so handle hidden paths specially.
    ;; this is fixed in later Racket versions with `path-replace-extension`
    (if (path-hidden? path)
        (do 'hide (path-replace-suffix (do 'unhide path) ""))
        (path-replace-suffix path ""))))


;; take all extensions off path
(define+provide+safe (remove-ext* x)
  (pathish? . -> . path?)
  (let loop ([path (->path x)])
    (define path-out (remove-ext path))
    (if (equal? path path-out)
        path
        (loop path-out))))