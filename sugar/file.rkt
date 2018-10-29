#lang racket/base
(require racket/list
         racket/match
         (except-in racket/path filename-extension)
         "define.rkt"
         "coerce/base.rkt")

;; this is identical to `filename-extension` in `racket/path`
;; but will not treat hidden files as an extension (which is a bug)
(define (filename-extension name)
  (match (file-name-from-path name)
    [(? path-for-some-system? filename)
     (=> resume)
     (match (regexp-match #rx#".[.]([^.]+)$" (path->bytes filename))
       [(list _ second) second]
       [_ (resume)])]
    [_ #false]))

(module+ test
  (require rackunit)
  (require (prefix-in rp: racket/path))
  (check-equal? (rp:filename-extension (string->path ".foo")) #"foo") ; bad behavior
  (check-false (filename-extension (string->path ".foo")))) ; good behavior

;; does path have a certain extension, case-insensitively
(define+provide+safe (has-ext? x ext)
  (pathish? stringish? . -> . boolean?)
  (unless (pathish? x)
    (raise-argument-error 'has-ext? "pathish?" x))
  (unless (stringish? ext)
    (raise-argument-error 'has-ext? "stringish?" ext))
  (define ext-of-path (filename-extension (->path x)))
  (and ext-of-path (string=? (string-downcase (bytes->string/utf-8 ext-of-path))
                             (string-downcase (->string ext)))))

;; get file extension as a string, or return #f 
;; (consistent with filename-extension behavior)
(define+provide+safe (get-ext x)
  (pathish? . -> . (or/c #f string?))
  (unless (pathish? x)
    (raise-argument-error 'get-ext "pathish?" x))
  (cond
    [(filename-extension (->path x)) => bytes->string/utf-8]
    [else #false]))

;; todo: add extensions
(provide+safe binary-extensions)
(define binary-extensions
  (map symbol->string '(gif jpg jpeg mp3 png zip pdf ico tar ai eps exe)))

(define+provide+safe (has-binary-ext? x)
  (pathish? . -> . boolean?)
  (unless (pathish? x)
    (raise-argument-error 'has-binary-ext? "pathish?" x))
  (for/or ([ext (in-list binary-extensions)]
           #:when (has-ext? (->path x) ext))
          #true))

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
  (and (regexp-match pat str) #true))

(define (path-hidden? path)
  ((->string (file-name-from-path path)) . starts-with? . "."))

(define (change-hide-state new-hide-state path)
  (define reversed-path-elements (reverse (explode-path path)))
  (apply build-path (append (reverse (cdr reversed-path-elements))
                            (list (if (eq? new-hide-state 'hide)
                                      (format ".~a" (->string (car reversed-path-elements)))
                                      (regexp-replace #rx"^." (->string (car reversed-path-elements)) ""))))))

;; take one extension off path
(define+provide+safe (remove-ext x)
  (pathish? . -> . path?)
  ;; `path-replace-suffix` incorrectly thinks any leading dot counts as a file extension
  ;; when it might be a hidden path.
  ;; so handle hidden paths specially.
  ;; this is fixed in later Racket versions with `path-replace-extension`
  (match (->path x)
    [(? path-hidden? path) (change-hide-state 'hide (path-replace-suffix (change-hide-state 'unhide path) ""))]
    [path (path-replace-suffix path "")]))

;; take all extensions off path
(define+provide+safe (remove-ext* x)
  (pathish? . -> . path?)
  (let loop ([path (->path x)])
    (match (remove-ext path)
      [(== path) path]
      [path-reduced (loop path-reduced)])))