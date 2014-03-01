#lang racket/base
(require (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax (try stx)
   (syntax-case stx ()
     [(_ body ... (except tests ...))
      #'(with-handlers (tests ...) body ...)]))

