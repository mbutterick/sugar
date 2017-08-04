#lang racket/base
(require (for-syntax racket/base) "define.rkt")

(provide+safe report report/line report/file
              report* report*/line report*/file
              report-apply repeat time-repeat time-repeat* compare)


(define-syntax (report stx)
  (syntax-case stx ()
    [(MACRO EXPR) #'(MACRO EXPR EXPR)]
    [(_ EXPR NAME)
     #'(let ([expr-result EXPR]) 
         (eprintf "~a = ~v\n" 'NAME expr-result)
         expr-result)]))


(define-syntax (report/line stx)
  (syntax-case stx ()
    [(MACRO EXPR) #'(MACRO EXPR EXPR)]
    [(_ EXPR NAME)
     #`(let ([expr-result EXPR])
         (eprintf "~a = ~v on line ~v\n" 'NAME expr-result #,(syntax-line #'EXPR))
         expr-result)]))


(define-syntax (report/file stx)
  (syntax-case stx ()
    [(MACRO EXPR) #'(MACRO EXPR EXPR)]
    [(_ EXPR NAME)
     #`(let ([expr-result EXPR])
         (eprintf "~a = ~v on line ~v in \"~a\"\n" 'NAME expr-result
                  #,(syntax-line #'EXPR)
                  '#,(syntax-source #'EXPR))
         expr-result)]))


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


(define-syntax-rule (repeat NUM EXPR ...)
  (for/last ([i (in-range NUM)])
            EXPR ...))


(define-syntax-rule (time-repeat NUM EXPR ...)
  (time (repeat NUM EXPR ...)))


(define-syntax (time-repeat* stx)
  (syntax-case stx ()
    [(_ NUM EXPR ...) 
     #'(let ([n NUM])
         (values (time-repeat n EXPR) ...))]))


(define-syntax (compare stx)
  (syntax-case stx ()
    [(_ EXPR ID ID-ALT ...) 
     #'(values EXPR (let ([ID ID-ALT]) EXPR) ...)]))

(module reader racket/base
  (require syntax/module-reader racket/syntax version/utils)  
  (provide (rename-out [debug-read read]
                       [debug-read-syntax read-syntax]
                       [debug-get-info get-info]))

  (define report-char #\R)
  
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
