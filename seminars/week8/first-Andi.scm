#lang scheme
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

(define (leaves tree)
  (if (empty-tree? tree) '()
    (cons (root-tree tree) (append (leaves (left-tree tree)) (leaves (right-tree tree))))))

(define (tree-sum tree)
  (apply + (leaves tree)))

(define test
  (make-tree 3
             (make-tree 1
                        (make-leaf 2)
                        empty-tree)
             (make-tree 5
                        (make-leaf 9)
                        (make-leaf 3))))
;(tree-sum test)

(define (tree-sum2 tree)
  (if (empty-tree? tree) 0
      (+ (root-tree tree)
         (tree-sum2 (left-tree tree))
         (tree-sum2 (right-tree tree)))))

;(tree-sum2 test)

(define (tree-height tree)
  (if (empty-tree? tree) 0
      (+ 1 (max (tree-height (left-tree tree)) (tree-height (right-tree tree))))))

;(tree-height test)

(define (tree-max tree)
  (apply max (leaves tree)))

;(tree-max test)

; -inf.0 и +inf.0
(define (tree-max2 tree)
  (if (empty-tree? tree) -inf.0
      (max (root-tree tree) (tree-max2 (left-tree tree)) (tree-max2 (right-tree tree)))))

(define (tree-level k t)
  (cond
    ((empty-tree? t) '())
    ((= k 0) (list (root-tree t)))
    (else (append (tree-level (- k 1) (left-tree t))
                  (tree-level (- k 1) (right-tree t))))))

(define (all-levels t)
  (map (lambda (i) (tree-level i t)) (range 0 (tree-height t))))

(define (tree-map f t)
  (if (empty-tree? t) '()
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))
                           
(define (tree->list t)
  (if (empty-tree? t) '()
  (append (tree->list (left-tree t)) (list (root-tree t)) (tree->list (right-tree t)))))

(define (bst-insert val t)
  (cond
    ((empty-tree? t) (make-leaf val))
    ((> val (root-tree t)) (make-tree (root-tree t)
                                      (left-tree t)
                                     (bst-insert val (right-tree t))))
    ;((= val (root-tree t)) t)
    (else (< val (root-tree t)) (make-tree (root-tree t)
                                      (bst-insert val (left-tree t))
                                      (right-tree t)))))

(define (list->tree l)
  (foldl bst-insert empty-tree l))

(define (list->tree2 l)
  (define (helper lst result)
    (if (null? lst) result
        (helper (cdr lst) (bst-insert (car lst) result))))
  (helper l empty-tree))

(define (tree-sort l)
  (tree->list (list->tree l)))

(define tree-sort* (compose tree->list list->tree))

;(define (parse-expression expr)

(define (valid-bst? t)
  (define (is-non-decreasing? l)
    (cond
      ((or (null? l) (null? (cdr l))) #t)
      ((> (car l) (cadr l)) #f)
      (else (is-non-decreasing? (cdr l)))))
  (is-non-decreasing? (tree->list t)))

; Др последната задача
; Всички!!! вляво са по-малки от корена
; Всички!!! вдясно са по-големи от корена


(define (valid-bst?? t)
  (define (helper t from to)
    (cond
      ((empty-tree? t) #t)
      ((or (< (root-tree t) from)
           (> (root-tree t) to)) #f)
      (else (and
             (helper (left-tree t) from (root-tree t))
             (helper (right-tree t) (root-tree t) to)))))
  (helper t -inf.0 +inf.0))

;number?;  list? cadr ; equal?
  