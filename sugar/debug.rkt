#lang racket/base
(require sugar/define)
(require-via-wormhole "../typed/sugar/debug.rkt")

(provide+safe report report/line report/file
              report* report*/line report*/file
              report-apply repeat time-repeat time-repeat* compare)


(module* reader racket/base
  (require syntax/module-reader racket/syntax)  
  (provide (rename-out [debug-read read]
                       [debug-read-syntax read-syntax]
                       [debug-get-info get-info]))
  
  (define (make-debug-readtable [rt (current-readtable)])
    (make-readtable rt #\^ 'dispatch-macro report-proc))

  
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
  
  
  (define-values (debug-read debug-read-syntax debug-get-info)
    (make-meta-reader
     'sugar/debug
     "language path"
     (lambda (bstr)
       (let* ([str (bytes->string/latin-1 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym) (vector `(submod ,sym reader)))))
     wrap-reader
     wrap-reader
     (lambda (proc)
       (lambda (key defval)
         (define (fallback) (if proc (proc key defval) defval))
         (case key
           [else (fallback)]))))))