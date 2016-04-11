#lang racket/base
(require "define.rkt" "coerce/base.rkt" racket/path)


;; does path have a certain extension
(define+provide+safe (has-ext? x ext)
  (pathish? stringish? . -> . boolean?)
  (define ext-of-path (filename-extension (->path x)))
  (->boolean (and ext-of-path (equal? (string-downcase (bytes->string/utf-8 ext-of-path)) (string-downcase (->string ext))))))


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


;; take one extension off path
(define+provide+safe (remove-ext x)
  (pathish? . -> . path?)
  ;; pass through hidden files (those starting with a dot)
  (let* ([x (->path x)]
         [x-name (file-name-from-path x)])
    (if ((->string x-name) . starts-with? . ".")
        x
        (path-replace-suffix x ""))))


;; take all extensions off path
(define+provide+safe (remove-ext* x)
  (pathish? . -> . path?)
  (define (remove* p)
    (let ([path-with-removed-ext (path-replace-suffix p "")])
      (if (equal? p path-with-removed-ext)
          p
          (remove* path-with-removed-ext))))
  ;; pass through hidden files (those starting with a dot)
  (let ([x (->path x)]
        [x-name (file-name-from-path x)])
    (if ((->string x-name) . starts-with? . ".")
        x
        (remove* x))))