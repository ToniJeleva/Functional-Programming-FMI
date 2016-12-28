#lang racket
(define (transpose m)
  (if (null? (car m)) '()
             (cons (map car m)(transpose (map cdr m)))))

(define (main-diag m)
  (if (null? m)'()
      (cons (caar m)(main-diag (map cdr (cdr m))))))

(define (2-nd-diag m)
  (main-diag (map reverse m)))

(define (triangular? m)
  (define (allZero col) ((all? (lambda (x) (= x 0)) col))
  (if (or (null? (car m)) (null? (cdr m))) 
      #t     
      (and (allZero (cdr (map car m)))
           (triangular? (cdr (map cdr m))))))

(define (descartes lst1 lst2)
  (apply append (map (lambda(x)(map (lambda(y)(cons x y)) lst2))lst1)))