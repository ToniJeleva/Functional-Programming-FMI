#lang racket
;fibonachi
(define (fib n)
  (if (= n 0)1
  (if (= n 1)1
  (+ (fib (- n 1)) (fib (- n 2))))))

;fibonachi iter
(define (fib-iter x)
  (define (fib2 f1 f2 x)
            (if (= x 0)
                f1
                (fib2 f2 (+ f2 f1) (- x 1))
            )
  )
  (fib2 0 1 x)
)

;sum
(define (sum n)
  (if (zero? n)
      0
      (+ n (sum (- n 1)))))

;fact
(define (fact n)
  (if (zero? n)
  1
  (* n (fact (- n 1)))))

;sum-iter
(define (sum-iter n)
  (define (sum2 n sum)
    (if (zero? n)
        sum
         (sum2 (- n 1) (+ sum n))))
  (sum2 n 0))

;fact-iter
(define (fact-iter n)
  (define (fact2 n res)
    (if (zero? n)
        res
        (fact2 (- n 1) (* res n))))
  (fact2 n 1))

;obrashta cifrite
(define (reverse-iter n)
  (define (helper n res)
    (if (= n 0)
        res
        (helper (quotient n 10) (+ (* res 10) (remainder n 10)))))
  (helper n 0))

;palindrome
;(define (palindrome? n)
 ; (= n (reverse-int-ter n)))

;sumanta na delitelite
(define (divisors-sum n)
  (define (helper n start res)
    (if (= n start) (+ res n)
        (if (= 0 (remainder n start)) (helper n (+ 1 start) (+ res start))
               (helper n (+ 1 start) res))))
  (helper n 1 0))

;perfect
(define (perfect? n)
  (= n (- (divisors-sum n) n)))

;prime
(define (prime? n)
  (define (helper i)
    (cond [(> i (sqrt n)) #t]
          [(= (remainder n i) 0) #f]
          [else (helper (+ i 1))]))
  (if (= n 1)
      #f
      (helper 2)))

;increasing
(define (increasing? n)
 (if (zero? n) #t
  (if (< (remainder n 10) (remainder (quotient n 10) 10)) #f
      (increasing? (quotient n 10)))))

