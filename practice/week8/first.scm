#lang scheme

(define empty-tree? null?)
(define root car)
(define left cadr)
(define right caddr)
(define (make-tree root left right)
  (list root left right))
(define (make-leaf root) (make-tree root '() '()))


(define test
  (make-tree 3
             (make-tree 1
                        (make-leaf 2)
                        '())
             (make-tree 5
                        (make-leaf 9)
                        (make-leaf 3))))
;(tree-sum test)

(define T '(5
            (3
             (1 () ())
             (4 () ()))
            (6
             () ())))

(define (sum-tree t)
  (if (empty-tree? t) 0
      (+ (root t) (sum-tree (left t)) (sum-tree (right t)))))

(sum-tree test)
(sum-tree T)

(define (level t l)
  (cond
    ((empty-tree? t) '())
    ((= 0 l) (list (root t)))
    (else (append (level (left t) (- l 1)) (level (right t) (- l 1))))))

; ще го поздравя с: Is this real or is it just fantasy?

(define (tree-map t f)
  (if (empty-tree? t) '()
      (make-tree (f (root t)) (tree-map (left t) f) (tree-map (right t) f))))


(define expr '(2 + 3)) ;-> дърво (+ (2 () ()) (3 () ())

(define expr2 '((7 + 4) - (2 * 3)))

(define (build-expr-tree expr)
  (cond
    ((empty-tree? expr) '())
    ((number? expr) (make-tree expr '() '()))
    (else (make-tree (left expr) (build-expr-tree (root expr)) (build-expr-tree (right expr))))))

(define (eval-expr-tree expr-tree)
  (if
    (number? (root expr-tree)) (root expr-tree)
    ((eval (root expr-tree)) (eval-expr-tree (left expr-tree)) (eval-expr-tree (right expr-tree)))))

