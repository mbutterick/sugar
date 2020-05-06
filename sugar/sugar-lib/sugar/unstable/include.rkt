#lang racket/base
(require (for-syntax racket/base
                     syntax/path-spec
                     racket/private/increader
                     compiler/cm-accomplice
                     racket/match racket/function)
         "../define.rkt")

(provide+safe include-without-lang-line)

(define-syntax (do-include stx)
  (syntax-case stx ()
    [(_ orig-stx ctx loc fn reader)
     ;; Parse the file name
     (let ([orig-c-file (resolve-path-spec (syntax fn) (syntax loc) (syntax orig-stx))]
           [ctx (syntax ctx)]
           [loc (syntax loc)]
           [reader (syntax reader)]
           [orig-stx (syntax orig-stx)]
           [rkt->ss (lambda (p)
                      (let ([b (path->bytes p)])
                        (if (regexp-match? #rx#"[.]rkt$" b)
                            (path-replace-suffix p #".ss")
                            p)))])
       
       (let ([c-file (if (file-exists? orig-c-file)
                         orig-c-file
                         (let ([p2 (rkt->ss orig-c-file)])
                           (if (file-exists? p2)
                               p2
                               orig-c-file)))])
         
         (register-external-file c-file)
         
         (let ([read-syntax (if (syntax-e reader)
                                (reader-val
                                 (let loop ([e (syntax->datum
                                                (local-expand reader 'expression null))])
                                   (cond
                                     [(reader? e) e]
                                     [(pair? e) (or (loop (car e))
                                                    (loop (cdr e)))]
                                     [else #f])))
                                (lambda (src in)
                                  (parameterize ([read-accept-reader #t])
                                    (read-syntax src in))))])
           (unless (and (procedure? read-syntax)
                        (procedure-arity-includes? read-syntax 2))
             (raise-syntax-error
              #f
              "reader is not a procedure of two arguments"
              orig-stx))
           
           ;; Open the included file
           (let ([p (with-handlers ([exn:fail?
                                     (lambda (exn)
                                       (raise-syntax-error
                                        #f
                                        (format
                                         "can't open include file (~a)"
                                         (if (exn? exn)
                                             (exn-message exn)
                                             exn))
                                        orig-stx
                                        c-file))])
                      (open-input-file c-file))])
             (port-count-lines! p)
             ;; Read expressions from file
             (let ([content
                    (let loop ()
                      (let ([r (with-handlers ([exn:fail?
                                                (lambda (exn)
                                                  (close-input-port p)
                                                  (raise-syntax-error
                                                   #f
                                                   (format
                                                    "read error (~a)"
                                                    (if (exn? exn)
                                                        (exn-message exn)
                                                        exn))
                                                   orig-stx))])
                                 (read-syntax c-file p))])
                        (if (eof-object? r)
                            null
                            (cons r (loop)))))])
               
               ;; Here's where we'll separate the content of the file from the #lang line.
               ;; the resulting material will be stored in 'content-guts'.
               ;; 'content' is a list of syntax objects from the source file.
               ;; Each object corresponds to a top-level expression in the file, converted to syntax.
               ;; If the file has a #lang line, there's only one expression (because the #lang expands to a single `module` form).
               ;; If it doesn't, then there are an indefinite number of expressions.
               ;; So we'll handle both types with a match.
               (define content-guts
                 (cond
                   [(not (null? content))
                    (define content-syntax (car content)) ; save the first syntax object (its context will be needed momentarily)
                    ;; peel the wrapper off the file. it will come in like so
                    ;; (module foo whatever/lang (#%module-begin expr ...))
                    ;; the guts are the (expr ...). To get them, we want the cdr of the fourth element. 
                    (define fourth cadddr) ; we don't have `fourth` in the syntax environment.
                    ;; get the guts and package them back into a syntax object using the saved content-syntax as context.
                    (define guts-data (match (map syntax->datum content)
                                        [(list (list 'module modname lang (list '#%module-begin exprs ...))) exprs]
                                        [(list exprs ...) exprs]))
                    (map (curry datum->syntax content-syntax) guts-data)]
                   [else null]))
               (close-input-port p)
               ;; Preserve src info for content, but set its
               ;; lexical context to be that of the include expression
               (let ([lexed-content
                      (let loop ([content content-guts]) ;; start with the new content-guts
                        (cond
                          [(pair? content)
                           (cons (loop (car content))
                                 (loop (cdr content)))]
                          [(null? content) null]
                          [else
                           (let ([v (syntax-e content)])
                             (datum->syntax
                              ctx
                              (cond
                                [(pair? v) 
                                 (loop v)]
                                [(vector? v)
                                 (list->vector (loop (vector->list v)))]
                                [(box? v)
                                 (box (loop (unbox v)))]
                                [else
                                 v])
                              content
                              content))]))])
                 *
                 (datum->syntax
                  (quote-syntax here)
                  `(begin ,@lexed-content)
                  orig-stx)))))))]))

(define-syntax (include-without-lang-line stx)
  (syntax-case stx ()
    [(_ fn)
     (with-syntax ([_stx stx])
       (syntax/loc stx (do-include _stx _stx _stx fn #f)))]))
