#lang racket/base
(require racket/contract racket/list)

(provide trim splitf-at*)

;; trim from beginning & end of list
(define (trim items test-proc)
  (list? procedure? . -> . list?)
  (dropf-right (dropf items test-proc) test-proc))


;; split list into list of sublists using test-proc
(define/contract (splitf-at* xs split-test)
    
  ;; todo: better error message when split-test is not a predicate
  (list? predicate/c . -> . (listof list?))
  (define (&splitf-at* xs [acc '()]) ; use acc for tail recursion
    (if (empty? xs) 
        ;; reverse because accumulation is happening backward 
        ;; (because I'm using cons to push latest match onto front of list)
        (reverse acc)
        (let-values ([(item rest) 
                      ;; drop matching elements from front
                      ;; then split on nonmatching 
                      ;; = nonmatching item + other elements (which will start with matching)
                      (splitf-at (dropf xs split-test) (compose1 not split-test))])
          ;; recurse, and store new item in accumulator
          (&splitf-at* rest (cons item acc)))))
  
  ;; trim off elements matching split-test
  (&splitf-at* (trim xs split-test)))