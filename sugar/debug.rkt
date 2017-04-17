#lang racket/base
(require (for-syntax racket/base racket/syntax) "define.rkt")

(provide+safe report report/line report/file
              report* report*/line report*/file
              report-apply repeat time-repeat time-repeat* compare)

(define-syntax (report stx)
  (syntax-case stx ()
    [(_ expr) #'(report expr expr)]
    [(_ expr name)
     #'(let ([expr-result expr]) 
         (eprintf "~a = ~v\n" 'name expr-result)
         expr-result)]))


(define-syntax (report/line stx)
  (syntax-case stx ()
    [(_ expr) #'(report/line expr expr)]
    [(_ expr name)
     (with-syntax ([line (syntax-line #'expr)])
       #'(let ([expr-result expr])
           (eprintf "~a = ~v on line ~v\n" 'name expr-result line)
           expr-result))]))


(define-syntax (report/file stx)
  (syntax-case stx ()
    [(_ expr) #'(report/file expr expr)]
    [(_ expr name)
     (with-syntax ([file (syntax-source #'expr)]
                   [line (syntax-line #'expr)])
       #'(let ([expr-result expr])
           (eprintf "~a = ~v on line ~v in \"~a\"\n" 'name expr-result line 'file)
           expr-result))]))


(define-syntax-rule (define-multi-version multi-name name)
  (define-syntax-rule (multi-name x (... ...))
    (begin (name x) (... ...))))

(define-multi-version report* report)
(define-multi-version report*/line report/line)
(define-multi-version report*/file report/file)


(define-syntax report-apply
  (syntax-rules ()
    [(report-apply proc expr) 
     (let ([lst expr])
       (report (apply proc lst) (apply proc expr)) 
       lst)]
    [(report-apply proc expr #:line)
     (let ([lst expr])
       (report (apply proc lst) (apply proc expr) #:line)
       lst)]))

#|
(define-syntax (verbalize stx)
  (syntax-case stx ()
    [(_ proc args ...)
     (with-syntax ([proc-input (format-id stx "args to ~a" #'proc)])
     #'(begin
         (let () (report (list args ...) proc-input) (void))
         (report (proc args ...))))]))
|#




(define-syntax-rule (repeat num expr ...)
  (for/last ([i (in-range num)])
    expr ...))


(define-syntax-rule (time-repeat num expr ...)
  (time (repeat num expr ...)))


(define-syntax (time-repeat* stx)
  (syntax-case stx ()
    [(_ num expr ...) 
     #'(let ([n num])
         (values (time-repeat n expr) ...))]))


(define-syntax (compare stx)
  (syntax-case stx ()
    [(_ expr id id-alt ...) 
     #'(values expr (let ([id id-alt]) expr) ...)]))

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
