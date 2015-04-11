#lang typed/racket/base
(require racket/port)
#;(provide (all-defined-out))
#;(require/typed xml [permissive-xexprs (Parameterof Boolean)]
               [#:struct prolog ([misc : (Listof Misc)][dtd : (Option DTD)][misc2 : (Listof Misc)])]
               [#:struct document ([prolog : Prolog][element : Element][misc : (Listof Misc)])])

#|
The following grammar describes expressions that create X-expressions:

  xexpr	 	=	 	string
 	 	|	 	(list symbol (list (list symbol string) ...) xexpr ...)
 	 	|	 	(cons symbol (list xexpr ...))
 	 	|	 	symbol
 	 	|	 	valid-char?
 	 	|	 	cdata
 	 	|	 	misc
|#

(define-type Cdata String) ;; could be tighter

;; valid-char could be tighter
#|
Returns true if x is an exact-nonnegative-integer whose character interpretation under UTF-8 is from the set ([#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]),
|#
(define-type Valid-Char Char) 
(define-type Xexpr (Rec X (U String
                             (List* Symbol (Listof (List Symbol String)) (Listof X))
                             (Pairof Symbol (Listof X))
                             Symbol
                             Valid-Char
                             Cdata)))
(define-predicate Xexpr? Xexpr)

#|
(: xml-string->xexprs (String . -> . (values Xexpr Xexpr)))
(define (xml-string->xexprs str)
  (define xml-doc (with-input-from-string str (λ _ (permissive-xexprs #t) (read-xml))))
  (values (xml->xexpr (document-prolog xml-doc)) (xml->xexpr (document-element xml-doc))))


(define (xexprs->xml-string prolog-xexpr root-xexpr)
  (xexpr? xexpr? . -> . string?)
  (with-output-to-string (λ _ (write-xml (document (xexpr->xml prolog-xexpr) (xexpr->xml root-xexpr) null)))))
|#