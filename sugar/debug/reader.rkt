#lang racket/base

(provide make-debug-readtable wrap-reader)

(require racket/syntax)

(define (make-debug-readtable [rt (current-readtable)])
  (make-readtable rt
    #\^ 'dispatch-macro report-proc
    ))

(define/with-syntax report (datum->syntax #f 'report))
(define/with-syntax report/line (datum->syntax #f 'report/line))

(define (report-proc c in src ln col pos)
  (define c2 (peek-char in))
  (cond [(char=? c2 #\^)
         (read-char in)
         (define stx (read-syntax/recursive src in))
         #`(report/line #,stx)]
        [else
         (define stx (read-syntax/recursive src in))
         #`(report #,stx)]))

(define (wrap-reader reader)
  (define (rd . args)
    (parameterize ([current-readtable (make-debug-readtable (current-readtable))])
      (apply reader args)))
  rd)

