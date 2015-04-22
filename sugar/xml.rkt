#lang racket/base
(require xml racket/port racket/contract "define.rkt")
(provide (all-defined-out))

(define+provide+safe (xml-string->xexprs str)
  (string? . -> . (values xexpr? xexpr?))
  (define xml-doc (with-input-from-string str (λ _ (permissive-xexprs #t) (read-xml))))
  (values (xml->xexpr (document-prolog xml-doc)) (xml->xexpr (document-element xml-doc))))

(define+provide+safe (xexprs->xml-string prolog-xexpr root-xexpr)
  (xexpr? xexpr? . -> . string?)
  (with-output-to-string (λ _ (write-xml (document (xexpr->xml prolog-xexpr) (xexpr->xml root-xexpr) null)))))