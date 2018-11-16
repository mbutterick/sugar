#lang racket/base
(require (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

;; like case but strictly uses `eq?` comparison (as opposed to `equal?`)
(define-syntax caseq (make-rename-transformer #'case))
(define-syntax casev (make-rename-transformer #'case))


(require sugar/debug)
(define-syntax (cond-report stx)
  (syntax-case stx ()
    [(_ [COND . BODY] ... [else . ELSE-BODY]) #'(cond [(report COND) (report (let () (void) . BODY))] ... [else . ELSE-BODY])]
    [(_ [COND . BODY] ... ) #'(cond-report [COND . BODY] ... [else (void)])])) 
