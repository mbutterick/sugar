#lang racket/base
(require sugar/define)
(require-via-wormhole "../typed/sugar/debug.rkt")

(provide+safe report report/line report/file
              report* report*/line report*/file
              report-apply repeat time-repeat time-repeat* compare)



(module reader racket/base
  (require (only-in syntax/module-reader make-meta-reader)
           racket/syntax
           version/utils
           syntax/parse/define
           (for-syntax racket/base racket/list))
  (provide (rename-out [debug-read read]
                       [debug-read-syntax read-syntax]
                       [debug-get-info get-info]))

  (define report-char #\R)

  (define-simple-macro (require-a-lot require-spec)
    #:with [i ...] (range -10 11)
    (require (for-meta i require-spec) ...))

  (require-a-lot racket/base)
  
  (define (make-debug-readtable [rt (current-readtable)])
    (make-readtable rt report-char 'dispatch-macro report-proc))

  
  (define (wrap-reader reader)
    (define (rd . args)
      (define intro (make-syntax-introducer))
      (parameterize ([current-readtable (make-debug-readtable (current-readtable))]
                     [current-syntax-introducer intro])
        (define stx (apply reader args))
        (if (and (syntax? stx) (version<=? "6.2.900.4" (version)))
            (intro stx)
            stx)))
    rd)

  
  (define current-syntax-introducer
    (make-parameter (λ (x) x)))

  
  (define (report-proc c in src ln col pos)
    (define c2 (peek-char in))
    (define c3 (peek-char in 1))
    (define intro (current-syntax-introducer))
    (cond [(and (char=? c3 report-char) (char=? c2 report-char))
           (read-char in)
           (read-char in)
           (define/with-syntax stx (intro (read-syntax/recursive src in)))
           (intro
            #'(let ()
                (local-require (only-in sugar/debug [report/file report/file]))
                (report/file stx)))]
          [(char=? c2 report-char)
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

  
  (define-values (debug-read debug-read-syntax debug-get-info)
    (make-meta-reader
     'sugar/debug
     "language path"
     (lambda (bstr)
       (let* ([str (bytes->string/latin-1 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym)
              (vector
               ;; try submod first:
               `(submod ,sym reader)
               ;; fall back to /lang/reader:
               (string->symbol (string-append str "/lang/reader"))))))
     wrap-reader
     wrap-reader
     (lambda (proc)
       (lambda (key defval)
         (define (fallback) (if proc (proc key defval) defval))
         (case key
           [else (fallback)]))))))
