#lang racket/base
(require (for-syntax racket/base))

(provide try)

;; Pythonlike try/except 
(define-syntax (try stx)
   (syntax-case stx ()
     [(_ body ... (except tests ...))
      #'(with-handlers (tests ...) body ...)]))


