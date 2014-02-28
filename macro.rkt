#lang racket/base
(require (for-syntax racket/base))

(provide macro-map macro-for-each)


(define-syntax-rule (make-mappy-macro mappy-macro-name joining-proc ending-value)
  (define-syntax mappy-macro-name
    (syntax-id-rules ()
      ;; convert quote form into list form
      [(_ macro-name (quote (items (... ...))))
       (mappy-macro-name macro-name (list items (... ...)))]
      
      ;; catch this case first, because it would also match the next one
      [(_ macro-name (list item0))
       (joining-proc 
        (macro-name item0) ending-value)]
      
      [(_ macro-name (list item0 items (... ...)))
       (joining-proc 
        (macro-name item0) 
        (mappy-macro-name macro-name (list items (... ...))))])))

(make-mappy-macro macro-for-each begin (void))
(make-mappy-macro macro-map cons '())



(define-syntax-rule (add pair)
  (+ (car pair) (cdr pair)))


;; this matches first case - why?
(macro-for-each add (list (cons 12 20)))


;(macro-map add (list 24 25 30))
;(macro-map add '(24 25 30))


;(macro-map-old add (list 24 25 30))

;(macro-for-each add '(24 25 30))

