#lang racket/base

(provide make-debug-readtable wrap-reader)

(require racket/syntax)

(define (make-debug-readtable [rt (current-readtable)])
  (make-readtable rt
    #\^ 'dispatch-macro report-proc
    ))

(define current-syntax-introducer
  (make-parameter (λ (x) x)))

(define (report-proc c in src ln col pos)
  (define c2 (peek-char in))
  (define intro (current-syntax-introducer))
  (cond [(char=? c2 #\^)
         (read-char in)
         (define/with-syntax stx (intro (read-syntax/recursive src in)))
         (intro
          #'(let ()
              (local-require (only-in sugar/debug [report/line report/line]))
              (report/line stx)))]
        [else
         (define/with-syntax stx (intro (read-syntax/recursive src in)))
         (intro
          #'(let ()
              (local-require (only-in sugar/debug [report report]))
              (report stx)))]))

(define (wrap-reader reader)
  (define (rd . args)
    (define intro (make-syntax-introducer))
    (parameterize ([current-readtable (make-debug-readtable (current-readtable))]
                   [current-syntax-introducer intro])
      (define stx (apply reader args))
      (if (syntax? stx)
          (intro stx)
          stx)))
  rd)

