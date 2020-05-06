#lang racket/base
(require (for-syntax racket/base racket/syntax) racket/class)
(provide (all-defined-out))

(define string%
  (class* object% (writable<%>)
    (super-new)
    (init-field [data #f])
    (define (get-string)
      (with-handlers ([exn:fail:object? (λ (exn) data)])
        (send this toString)))
    (define/public (custom-write port) (write (get-string) port))
    (define/public (custom-display port) (display (get-string) port))))

(define mixin-tester%
  (class object%
    (super-new)
    (define/public (addContent val) (make-object string% val))))

(define-syntax (as-method stx)
  (syntax-case stx ()
    [(_ ID) (with-syntax ([PRIVATE-ID (generate-temporary #'ID)])
              #'(begin
                  (public [PRIVATE-ID ID])
                  (define (PRIVATE-ID . args) (apply ID this args))))]))


(define-syntax-rule (as-methods ID ...)
  (begin (as-method ID) ...))


(define-syntax (define-instance stx)
  (syntax-case stx ()
    [(_ ID (MAKER BASE-CLASS . ARGS))
     (with-syntax ([ID-CLASS (format-id stx "~a:~a" (syntax->datum #'BASE-CLASS) (syntax->datum #'ID))])
       #'(define ID (let ([ID-CLASS (class BASE-CLASS (super-new))])
                      (MAKER ID-CLASS . ARGS))))]))


(define-syntax (define-class-predicates stx)
  (syntax-case stx ()
    [(_ ID)
     (with-syntax ([+ID (format-id #'ID "+~a" (syntax->datum #'ID))]
                   [ID? (format-id #'ID "~a?" (syntax->datum #'ID))])
       #'(begin (define (ID? x) (is-a? x ID))
                (define (+ID . args) (apply make-object ID args))))]))

(define-syntax-rule (define-subclass*/interfaces SUPERCLASS INTERFACES (ID . INIT-ARGS) . BODY)
  (begin
    (define ID (class* SUPERCLASS INTERFACES (init-field . INIT-ARGS) . BODY))
    (define-class-predicates ID)))

(define-syntax-rule (define-subclass/interfaces SUPERCLASS INTERFACES (ID . INIT-ARGS) . BODY)
  (define-subclass*/interfaces SUPERCLASS INTERFACES (ID . INIT-ARGS) (super-new) . BODY))

(define-syntax-rule (define-subclass* SUPERCLASS (ID . INIT-ARGS) . BODY)
  (define-subclass*/interfaces SUPERCLASS () (ID . INIT-ARGS) . BODY))

(define-syntax-rule (define-subclass SUPERCLASS (ID . INIT-ARGS) . BODY)
  (define-subclass* SUPERCLASS (ID . INIT-ARGS) (super-new) . BODY))


(define-syntax-rule (push-field! FIELD O EXPR)
  (set-field! FIELD O (cons EXPR (get-field FIELD O))))


(define-syntax-rule (push-end-field! FIELD O EXPR)
  (set-field! FIELD O (append (get-field FIELD O) (list EXPR))))

(define-syntax-rule (pop-field! FIELD O)
  (let ([xs (get-field FIELD O)])
    (set-field! FIELD O (cdr xs))
    (car xs)))

(define-syntax (increment-field! stx)
  (syntax-case stx ()
    [(_ FIELD O)  #'(increment-field! FIELD O 1)]
    [(_ FIELD O EXPR)
     #'(begin (set-field! FIELD O (+ (get-field FIELD O) EXPR)) (get-field FIELD O))]))


(define-syntax (getter-field/override stx)
  (syntax-case stx ()
    [(_ [ID . EXPRS])
     (syntax-property #'(getter-field [ID . EXPRS]) 'override #t)]))


(define-syntax (getter-field stx)
  (syntax-case stx ()
    [(_ [ID . EXPRS])
     (with-syntax ([_ID (format-id #'ID "_~a" (syntax->datum #'ID))])
       #`(begin
           (field [(ID _ID)  . EXPRS])
           (public (_ID ID))
           (#,(if (syntax-property stx 'override) #'define/override #'define) (_ID) ID)))]))