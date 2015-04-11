#lang typed/racket/base/no-check
(require (for-syntax typed/racket/base) typed/rackunit)

(provide check-typing-fails check-typing)

(define-syntax (check-typing-base stx)
  (syntax-case stx ()
    [(_ wants-to-fail? expr)
     (let* ([wants-to-fail? (syntax->datum #'wants-to-fail?)]
            [λ-arg 'v]
            [eval-string (if wants-to-fail? `(cons '#%top-interaction ,λ-arg) λ-arg)]
            [check-string (if wants-to-fail? '(curry check-exn exn:fail:syntax?) 'check-not-exn)])
       #`(begin
           (define-namespace-anchor ns)
           (let ([E (λ(#,λ-arg) (eval #,eval-string (namespace-anchor->namespace ns)))])
             (apply #,check-string (list (λ _ (call-with-values (λ _ (E 'expr)) (λ vals (car vals)))))))))]))

(define-syntax-rule (check-typing-fails expr)
  (check-typing-base #t expr))

(define-syntax-rule (check-typing expr)
  (check-typing-base #f expr))
