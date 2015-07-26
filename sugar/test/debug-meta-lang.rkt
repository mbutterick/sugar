#lang sugar/debug/lang racket
(require rackunit)
(let ([out (open-output-string)]
      [let "something else"]
      [local-require "something else entirely"]
      [only-in "completely unexpected!"]
      [report "well, not really"])
  (parameterize ([current-error-port out])
    #^5)
  (check-equal? (get-output-string out) "5 = 5\n"))
(let ([out (open-output-string)]
      [report/line "outta the blue!"])
  (parameterize ([current-error-port out])
    #^^5)
  (check-equal? (get-output-string out) "5 = 5 on line 14\n"))
