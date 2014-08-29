#lang racket/base
(require "define.rkt" "coerce.rkt" "string.rkt" racket/path)

(define+provide/contract (get-enclosing-dir p)
  (coerce/path? . -> . path?)
  (simplify-path (build-path p 'up)))

;; does path have a certain extension
(define+provide/contract (has-ext? x ext)
  (coerce/path? coerce/string? . -> . coerce/boolean?)
  (define ext-of-path (filename-extension x))
  (and ext-of-path (equal? (string-downcase (bytes->string/utf-8 ext-of-path)) (string-downcase ext))))

;; get file extension as a string, or return #f 
;; (consistent with filename-extension behavior)
(define+provide/contract (get-ext x)
  (coerce/path? . -> . (or/c #f string?))
  (let ([fe-result (filename-extension x)])
    (and fe-result (bytes->string/utf-8 fe-result))))


;; todo: add extensions
(define binary-extensions
  (map ->string '(gif jpg jpeg mp3 png zip pdf ico tar ai eps exe)))

(define+provide/contract (has-binary-ext? x)
  (coerce/path? . -> . coerce/boolean?)
  (ormap (λ(ext) (has-ext? x ext)) binary-extensions))

;; put extension on path
;; use local contract here because this function is used within module
(define/contract+provide (add-ext x ext)
  (coerce/string? coerce/string? . -> . coerce/path?)
  (string-append x "." ext))

;; take one extension off path
(define+provide/contract (remove-ext x)
  (coerce/path? . -> . path?)
  ;; pass through hidden files (those starting with a dot)
  (if (x . starts-with? . ".")
      x
      (path-replace-suffix x "")))


;; take all extensions off path
(define+provide/contract (remove-ext* x)
  (coerce/path? . -> . path?)
  ;; pass through hidden files (those starting with a dot)
  (if (x . starts-with? . ".")
      x
      (let ([path-with-removed-ext (remove-ext x)])
        (if (equal? x path-with-removed-ext)
            x
            (remove-ext* path-with-removed-ext)))))

