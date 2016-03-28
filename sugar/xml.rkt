#lang racket/base
(require xml racket/port racket/contract "define.rkt")
(provide (all-defined-out))

(define+provide+safe (xml-string->xexprs str)
  (string? . -> . (values xexpr? xexpr?))
  (define xml-doc (with-input-from-string str
                                          (λ () (permissive-xexprs #t) (read-xml))))
  (values (xml->xexpr (document-prolog xml-doc)) (xml->xexpr (document-element xml-doc))))


(define+provide+safe (xexprs->xml-string prolog-xexpr root-xexpr)
  (xexpr? xexpr? . -> . string?)
  (with-output-to-string (λ () (write-xml (document (xexpr->xml prolog-xexpr) (xexpr->xml root-xexpr) null)))))


(module+ test
  (require rackunit)
  (require xml)
  (define str "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<root>hello world</root>")
  (define-values (str-prolog str-doc) (xml-string->xexprs str))
  (check-equal? str-prolog (prolog (list (p-i (location 1 0 1) (location 1 38 39) 'xml "version=\"1.0\" encoding=\"utf-8\"")) #f null))
  (check-equal? str-doc '(root () "hello world"))
  (check-equal? (xexprs->xml-string str-prolog str-doc) str))
