#lang racket/base
(require (for-syntax racket/base))
(require "define/contract.rkt")

(define+provide/contract (bytecount->string bytecount)
  (integer? . -> . string?)
  (define (format-with-threshold threshold suffix)
    ;; upconvert by factor of 100 to get two digits after decimal
    (format "~a ~a" (exact->inexact (/ (round ((* bytecount 100) . / . threshold)) 100)) suffix))
  
  (define threshold-kilobyte 1000)
  (define threshold-megabyte (threshold-kilobyte . * . threshold-kilobyte))
  (define threshold-gigabyte (threshold-megabyte . * . threshold-kilobyte))
  (define threshold-terabyte (threshold-gigabyte . * . threshold-kilobyte))
  
  (cond
    [(bytecount . >= . threshold-terabyte) (format-with-threshold threshold-terabyte "TB")]
    [(bytecount . >= . threshold-gigabyte) (format-with-threshold threshold-gigabyte "GB")]
    [(bytecount . >= . threshold-megabyte) (format-with-threshold threshold-megabyte "MB")]
    [(bytecount . >= . threshold-kilobyte) (format-with-threshold threshold-kilobyte "KB")]
    [else (format "~a bytes" bytecount)]))


;; for use inside quasiquote
;; instead of ,(when ...) use ,@(when/splice ...)
;; to avoid voids
(provide when/splice)
(define-syntax (when/splice stx)
  (syntax-case stx ()
    [(_ test body)
      #'(if test (list body) '())]))