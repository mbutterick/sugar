#lang typed/racket/base
(require typed/sugar/define typed/sugar/coerce typed/sugar/string racket/path)

(define/typed+provide (get-enclosing-dir p)
  (Pathish -> Path)
  (simplify-path (build-path (->path p) 'up)))

;; does path have a certain extension
(define/typed+provide (has-ext? x ext)
  (Pathish Stringish -> Boolean)
  (define ext-of-path (filename-extension (->path x)))
  (->boolean (and ext-of-path (equal? (string-downcase (bytes->string/utf-8 ext-of-path)) (string-downcase (->string ext))))))

;; get file extension as a string, or return #f 
;; (consistent with filename-extension behavior)
(define/typed+provide (get-ext x)
  (Pathish -> (Option String))
  (let ([fe-result (filename-extension (->path x))])
    (and fe-result (bytes->string/utf-8 fe-result))))


;; todo: add extensions
(define/typed+provide binary-extensions
  (Listof String)
  (map symbol->string '(gif jpg jpeg mp3 png zip pdf ico tar ai eps exe)))

(define/typed+provide (has-binary-ext? x)
  (Pathish -> Boolean)
  (let ([x (->path x)])
    (ormap (λ:([ext : String]) (has-ext? x ext)) binary-extensions)))

;; put extension on path
;; use local contract here because this function is used within module
(define/typed+provide (add-ext x ext)
  (Stringish Stringish -> Path)
  (->path (string-append (->string x) "." (->string ext))))

;; take one extension off path
(define/typed+provide (remove-ext x)
  (Pathish -> Path)
  ;; pass through hidden files (those starting with a dot)
  (let ([x (->path x)])
    (if (x . starts-with? . ".")
        x
        (path-replace-suffix x ""))))


;; take all extensions off path
(define/typed+provide (remove-ext* x)
  (Pathish -> Path)
  ;; pass through hidden files (those starting with a dot)
  (let ([x (->path x)])
    (if (x . starts-with? . ".")
        x
        (let ([path-with-removed-ext (remove-ext x)])
          (if (equal? x path-with-removed-ext)
              x
              (remove-ext* path-with-removed-ext))))))

