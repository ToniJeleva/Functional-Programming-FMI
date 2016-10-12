#lang racket
(define (cut-last-digit n)
  (remainder n 10))
(define (cut-number-from-last-digit n)
  (quotient n 10))
(define (multiply-digits n)
  (cond
    [(zero? n) 1]
    [else (*(cut-last-digit n)(multiply-digits(cut-number-from-last-digit n)))]))