#lang racket
(define (meetTwice f g a b)
  (define (helper f g a b mem)
    (if (> a b) (if(> 2 (length mem)) #f
                   (if (= (car mem) (cadr mem)) #f #t))
        (if (= (f a) (g a)) (helper f g (+ 1 a) b (cons a mem))
            (helper f g (+ 1 a) b mem))))
  (helper f g a b '()))