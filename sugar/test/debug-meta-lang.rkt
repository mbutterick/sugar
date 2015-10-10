#lang sugar/debug racket
(require rackunit
         (for-meta 1 (only-in racket/base begin-for-syntax))
         (for-meta 2 (only-in racket/base begin-for-syntax))
         (for-meta 3 (only-in racket/base let #%app open-output-string get-output-string parameterize
                              current-error-port #%datum)
                   rackunit))
(let ([out (open-output-string)]
      [let "something else"]
      [local-require "something else entirely"]
      [only-in "completely unexpected!"]
      [report "well, not really"])
  (parameterize ([current-error-port out])
    #R5)
  (check-equal? (get-output-string out) "5 = 5\n"))
(let ([out (open-output-string)]
      [report/line "outta the blue!"])
  (parameterize ([current-error-port out])
    #RR5)
  (check-equal? (get-output-string out) "5 = 5 on line 19\n"))

(begin-for-syntax
  (begin-for-syntax
    (begin-for-syntax
      (let ([out (open-output-string)])
        (parameterize ([current-error-port out])
          #RR5)
        (check-equal? (get-output-string out) "5 = 5 on line 27\n")))))
