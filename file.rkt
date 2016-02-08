#lang racket/base
(require "define.rkt" racket/set "coerce.rkt" racket/path "unstable/string.rkt")


(define+provide+safe (get-enclosing-dir p)
  (coerce/path? . -> . path?)
  (simplify-path (build-path (->path p) 'up)))


;; does path have a certain extension
(define+provide+safe (has-ext? x ext)
  (coerce/path? coerce/string? . -> . coerce/boolean?)
  (define ext-of-path (filename-extension (->path x)))
  (->boolean (and ext-of-path (equal? (string-downcase (bytes->string/utf-8 ext-of-path)) (string-downcase (->string ext))))))


;; get file extension as a string, or return #f 
;; (consistent with filename-extension behavior)
(define+provide+safe (get-ext x)
  (coerce/path? . -> . (or/c #f string?))
  (let ([fe-result (filename-extension (->path x))])
    (and fe-result (bytes->string/utf-8 fe-result))))


;; todo: add extensions
(provide+safe binary-extensions)
(define binary-extensions
  (map symbol->string '(gif jpg jpeg mp3 png zip pdf ico tar ai eps exe)))


(define+provide+safe (has-binary-ext? x)
  (coerce/path? . -> . coerce/boolean?)
  (let ([x (->path x)])
    (ormap (λ(ext) (has-ext? x ext)) binary-extensions)))


;; put extension on path
;; use local contract here because this function is used within module
(define+provide+safe (add-ext x ext)
  (coerce/string? coerce/string? . -> . coerce/path?)
  (->path (string-append (->string x) "." (->string ext))))

;; take one extension off path
(define+provide+safe (remove-ext x)
  (coerce/path? . -> . path?)
  ;; pass through hidden files (those starting with a dot)
  (let ([x (->path x)])
    (if ((->string x) . starts-with? . ".")
        x
        (path-replace-suffix x ""))))


;; take all extensions off path
(define+provide+safe (remove-ext* x)
  (coerce/path? . -> . path?)
  ;; pass through hidden files (those starting with a dot)
  (let ([x (->path x)])
    (if ((->string x) . starts-with? . ".")
        x
        (let ([path-with-removed-ext (remove-ext x)])
          (if (equal? x path-with-removed-ext)
              x
              (remove-ext* path-with-removed-ext))))))