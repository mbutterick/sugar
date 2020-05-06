#lang racket/base
(require (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

(begin-for-syntax
  (require racket/string racket/format)
  (define (make-prefix caller-stx)
    (string-join (map ~a (list (syntax-source caller-stx) (syntax-line caller-stx))) ":" #:after-last ":")))

(define-syntax (define-stub-stop stx)
  (syntax-case stx ()
    [(_ ID)
     (with-syntax ([ERROR-ID (format-id stx "~a~a:not-implemented" (make-prefix stx) (syntax->datum #'ID))])
       #'(define (ID . args)
           (error 'ERROR-ID)))]))

(provide (rename-out [define-stub-stop define-stub]))

(define-syntax (define-stub-go stx)
  (syntax-case stx ()
    [(_ ID)
     (with-syntax ([ERROR-ID (format-id stx "~a~a:not-implemented" (make-prefix stx) (syntax->datum #'ID))])
       #'(define (ID . args)
           (displayln 'ERROR-ID)))]))

(define-syntax (define-unfinished stx)
  (syntax-case stx ()
    [(_ (ID . ARGS) . BODY)
     (with-syntax ([ID-UNFINISHED (format-id stx "~a~a:unfinished" (make-prefix stx) (syntax->datum #'ID))])
       #'(define (ID . ARGS)
           (begin . BODY)
           (error 'ID-UNFINISHED)))]))


(define-syntax (unfinished stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([ID-UNFINISHED (format-id stx "~a:~a:~a" (path->string (syntax-source stx)) (syntax-line stx) (syntax->datum #'unfinished))])
       #'(error 'ID-UNFINISHED))]))