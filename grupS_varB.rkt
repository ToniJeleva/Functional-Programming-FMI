#lang racket
;zad 2
;sumata na vsqka podredica
(define (find-sum ls)
  (map (lambda(i)(foldr + 0 i)) ls))

;dali element se sadarja v spisak
(define (contains? ls el)
  (if (= 0 (length (filter (lambda(x)(if(= el x) #t #f)) ls))) #f #t))

;na vsqko ot chislata v spisaka supostava #t #f v zavisimost ot dali se sadarja v spiska na sumite na podspisacite
;posle ako ima takova chislo(chiqto stoinost e mapnata na #t) vrashta rezultata
(define (checksum list)
 (if (< 0 (length(filter (lambda(x) x) (foldr append '() (map (lambda(x)(map (lambda(i)(if(contains? (find-sum list) i) #t #f)) x)) list)))))#t #f))

;zad3
;dali spisak e rastqshta redica
(define (is-increasing ls)
  (define (helper ls previous res)
    (if (null? ls) res
        (if (< previous (car ls)) (helper (cdr ls) (car ls) (cons #t res))
            (helper (cdr ls) (car ls) (cons #f res)))))
  (if(< 0 (length (filter (lambda(x)(not x)) (helper (cdr ls) (car ls) '())))) #f #t))

;pravi spisak ot spisaci,koito sa kolonite na matrica
(define (combine-col ls len res)
 (if(> 0 len) res
 (combine-col ls (- len 1) (cons(map (lambda(i)(list-ref i len)) ls) res))))
;proverqva dali vsqka kolona e rastastqsha i ako ima edna takava vrashta #t
(define (exists-increasing? list)
  (if(< 0 (length(filter (lambda(i)i)(map (lambda(i)(is-increasing i)) (combine-col list (- (length list) 1) '())))))#t #f))

