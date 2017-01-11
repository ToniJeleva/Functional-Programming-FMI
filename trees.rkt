#lang racket
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      
(define (make-leaf root) (make-tree root empty-tree empty-tree)) 
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define (is-leaf? t)(and (empty-tree? left-tree)(empty-tree? right-tree)))

(define test
  (make-tree 3
             (make-tree 1
                        (make-leaf 2)
                        empty-tree)
             (make-tree 5
                        (make-leaf 9)
                        (make-leaf 3))))


(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t)
         (tree-sum (left-tree t))
         (tree-sum (right-tree t)))))

(define (tree-max t)
  (if (empty-tree? t)
     -inf.0
     (max (root-tree t)
          (tree-max (left-tree t))
          (tree-max (right-tree t)))))

(define (tree-level k t)
  (cond [(empty-tree? t) '()] 
        [(= k 0)(list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))

;(define (all-levels t)
 ; (if (empty-tree? t) '()       not working - deep-flattern should be applied after
  ;    (cons (root-tree t)(cons
   ;          (all-levels (left-tree t))
    ;         (all-levels (right-tree t))))))



(define (height t)
  (if (empty-tree? t) -1
      (+ 1 (max (height (left-tree t)) (height (right-tree t))))))

;(define (all-levels' t)
 ; (let [(height (tree-height t))]
  ;  (map (lambda (i) (tree-level i t)) (range 0 (+ height 1)))))

(define (tree-map f t)
  (if (empty-tree? t) '()
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))

(define (tree->list t)
  (if (empty-tree? t) '()
      (append (tree->list (left-tree t))
              (list (root-tree t)) ;appends lists
              (tree->list (right-tree t)))))

