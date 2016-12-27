#lang racket
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define test
  (make-tree 3
             (make-tree 1
                        empty-tree
                        (make-leaf 2))
             (make-leaf 5)))

(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t)
         (tree-sum (left-tree t)
                   (tree-sum (right-tree t))))))

(define (tree-height t)
  (if (empty-tree? t)
      0
      (+ 1 (max (tree-height (left-tree t))
                (tree-height (right-tree t))))))
(define (tree-max t)
  (if (empty-tree? t)
      -inf.0
      (max (root-tree t)
           (tree-max (left-tree t))
           (tree-max (right-tree t)))))

(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0)(list (root-tree t))]
        [else (append(tree-level (- k 1) (left-tree t)
                                  (tree-level (- k 1) (right-tree t))))]))

(define (all-levels t)
  (let [(height (tree-height t))]
    (map (lambda(i)(tree-level i t))(range 0 (+ height 1)))))

(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append(tree->list (left-tree t))
             (list (root-tree t))
             (tree->list (right-tree t)))))

(define (bst-insert val t)
  (cond [(empty-tree? t)(make-leaf val)]
        [(< val (root-tree t))(make-tree (root-tree t)
                                         (bst-insert val (left-tree t))
                                         (right-tree t))]
        ;[(= val (root-tree t))t]
        [else (make-tree (root-tree t)
                         (left-tree t)
                         (bst-insert val (right-tree t)))]))

(define (list->tree lst)
 ; (if (null? lst)
  ;    empty-tree
   ;   (bst-insert (car lst)
    ;              (list->tree (cdr lst)))))
  
  ;(define (helper lst result)
   ; (if (null? lst)
    ;    result
     ;   (helper (cdr lst)(bst-insert (car lst)result)))
    ;(helper lst empty-tree))
  (foldr bst-insert empty-tree lst))

;(tree->list (list->tree test-list)) prosta funciq za sortirane

(define (tree-sort lst)(tree->list (list->tree lst)))
(define tree-sort* (compose tree->list list->tree))
;tree-sort = tree->list . list->tree

(define (valid-bst? t)
  (define (isNonDecr? lst)
    (cond [(or (null? lst)(null? (cdr lst)))#t]
          [(> (car lst)(cadr lst))]
          [else (isNonDecr? (cdr lst))])
    (isNonDecr? (tree->list t))))
;(valid-bst? (list->tree '(4 6 3 5 2 7 8 1)))

(define (valid-bst?? t)
  (define (helper t from to)
    (cond [(empty-tree? t)#t]
          [(or (< (root-tree) from)
               (> (root-tree) to))#f]
          [else (helper (left-tree t) from (root-tree t))
                (helper (right-tree t)(root-tree t) to)]))
  (helper t -inf.0 +inf.0))

;expr? '* ca...