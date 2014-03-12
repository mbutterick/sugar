#lang racket/base
(require racket/contract "../define/provide.rkt" "value.rkt")


(define-syntax-rule (make-blame-handler try-proc expected-sym)
  (λ(b)
    (λ(x)
      (with-handlers ([exn:fail? (λ(e)
                                   (raise-blame-error
                                    b x
                                    '(expected: "~a" given: "~e")
                                    expected-sym x))])
                      (try-proc x)))))

(define+provide coerce/integer?
  (make-contract
   #:name 'coerce/integer?
   #:projection (make-blame-handler ->int 'can-be-integer?)))


(define+provide coerce/string?
  (make-contract
   #:name 'coerce/string?
   #:projection (make-blame-handler ->string 'can-be-string?)))


(define+provide coerce/symbol?
  (make-contract
   #:name 'coerce/symbol?
   #:projection (make-blame-handler ->symbol 'can-be-symbol?)))


(define+provide coerce/path?
  (make-contract
   #:name 'coerce/path?
   #:projection (make-blame-handler ->path 'can-be-path?)))


(define+provide coerce/boolean?
  (make-contract
   #:name 'coerce/boolean?
   #:projection (make-blame-handler ->boolean 'can-be-boolean?)))




