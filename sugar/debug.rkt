#lang racket/base
(require racket/string (for-syntax racket/base) "define.rkt")

(provide+safe report report/time time-name
              report/line report/file
              report* report*/line report*/file
              report-apply repeat time-repeat time-repeat* compare)

(define (stringify-results expr-results)
  (format (if (= 1 (length expr-results))
              "~a"
              "(values ~a)") (string-join (for/list ([r (in-list expr-results)])
                                                    (format "~v" r)) " ")))

(define-syntax (report stx)
  (syntax-case stx ()
    [(MACRO EXPR) #'(MACRO EXPR EXPR)]
    [(_ EXPR NAME)
     #'(let ([expr-results (call-with-values (λ () EXPR) list)]) 
         (eprintf "~a = ~a\n" 'NAME (stringify-results expr-results))
         (apply values expr-results))]))


(define-syntax (report/time stx)
  (syntax-case stx ()
    [(MACRO EXPR) #'(MACRO EXPR EXPR)]
    [(_ EXPR NAME)
     #'(let* ([op (open-output-string)]
              [expr-results (parameterize ([current-output-port op])
                              (time (call-with-values (λ () EXPR) list)))]) 
         (eprintf "~a = ~a [~a]\n" 'NAME (stringify-results expr-results) (string-trim (get-output-string op)))
         (apply values expr-results))]))


(define-syntax (report/line stx)
  (syntax-case stx ()
    [(MACRO EXPR) #'(MACRO EXPR EXPR)]
    [(_ EXPR NAME)
     #`(let ([expr-results (call-with-values (λ () EXPR) list)])
         (eprintf "~a = ~a on line ~a\n" 'NAME (stringify-results expr-results) #,(syntax-line #'EXPR))
         (apply values expr-results))]))


(define-syntax (report/file stx)
  (syntax-case stx ()
    [(MACRO EXPR) #'(MACRO EXPR EXPR)]
    [(_ EXPR NAME)
     #`(let ([expr-results (call-with-values (λ () EXPR) list)])
         (eprintf "~a = ~a on line ~a in \"~a\"\n" 'NAME (stringify-results expr-results)
                  #,(syntax-line #'EXPR)
                  '#,(syntax-source #'EXPR))
         (apply values expr-results))]))


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


(define-syntax (time-name stx)
  (syntax-case stx ()
    [(_ NAME EXPR ...)
     #'(let* ([op (open-output-string)]
              [expr-results (parameterize ([current-output-port op])
                              (time (call-with-values (λ () EXPR ...) values)))])
              (display (format "~a: ~a" 'NAME (get-output-string op)))
              expr-results)]))


(define-syntax (compare stx)
  (syntax-case stx ()
    [(_ EXPR ID ID-ALT ...) 
     #'(values EXPR (let ([ID ID-ALT]) EXPR) ...)]))

(module reader racket/base
  (require syntax/module-reader racket/syntax version/utils)  
  (provide (rename-out [debug-read read]
                       [debug-read-syntax read-syntax]
                       [debug-get-info get-info]))

  (define current-metalang-scope-flipper (make-parameter values))

  (define (wrap-reader reader)
    (λ args
      (parameterize ([current-readtable (make-debug-readtable (current-readtable))]
                     [current-metalang-scope-flipper (make-syntax-introducer)])
        (define stx (apply reader args))
        (define proc (if (and (syntax? stx) (version<=? "6.2.900.4" (version)))
                         (current-metalang-scope-flipper)
                         values))
        (proc stx))))

  (define-values (debug-read debug-read-syntax debug-get-info)
    (make-meta-reader
     'sugar/debug
     "language path"
     (λ (bstr) ; copy of `lang-reader-module-paths`, only available since 6.7
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
     (λ (proc)
       (λ (key defval)
         (case key
           [else (if proc (proc key defval) defval)])))))

  (define report-char #\R)

  (define (make-debug-readtable [rt (current-readtable)])
    (make-readtable rt report-char 'dispatch-macro report-proc))

  (define (another-report-char? ip) (and (char=? (peek-char ip) report-char) (read-char ip)))
  
  (define (report-proc trigger-char ip src ln col pos)
    (define flip-metalang-scope (current-metalang-scope-flipper))
    (flip-metalang-scope (with-syntax ([REPORT-ID (cond
                                                    [(not (another-report-char? ip)) 'report] ; #R...
                                                    [(not (another-report-char? ip)) 'report/line] ; #RR...
                                                    [else 'report/file])] ; #RRR...
                                       [STX (flip-metalang-scope (read-syntax/recursive src ip))])
                           #'(let ()
                               (local-require (only-in sugar/debug REPORT-ID))
                               (REPORT-ID STX))))))
