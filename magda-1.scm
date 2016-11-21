#lang scheme
(define (range a b)
  (if (> a b) '()
      (cons a (range (+ 1 a) b))))
(define (sum-digits x)
  (if (= x 0) 0
      (+ (remainder x 10) (sum-digits (quotient x 10)))))

(define  (min-sum-digit a b k)
  (apply min (filter (lambda (x) (= 0 (remainder (sum-digits x) k))) (range a b))))


(min-sum-digit 1 10 2)

;----------

(define (average f g)
  (lambda (x) (/ (+ (f x) (g x)) 2)))

(define (accumulate op nv term a b next)
  (if (> a b) nv
      (op (term a) (accumulate op nv term (next a) b next))))

(define (g i)
  (lambda (x) (expt i x)))


(define (calcprod f n)
  (accumulate * 1 (lambda (i) ((average f (g i)) i)) 1 n (lambda (i) (+ 1 i))))

(calcprod (lambda (x) x) 3)


;----------

(define (occurrences-list val lst)
  (length (filter (lambda (el) (= val el)) lst)))

(define (occurrences lst1 lst2)
  (map (lambda (el) (occurrences-list el lst2)) lst1))

(occurrences '(1 2 3) '( 1 2 4 1 )); -> (2 1 0)

;-------------
(define (all? p? lst)
  (cond
    ((null? lst) #t)
    ((not (p? (car lst))) #f)
    (else (all? p? (cdr lst)))))

(define (all-equal? lst)
  (= (apply max lst) (apply min lst)))

(define (match-lengths? l1 l2)
  (all-equal? (map (lambda (x y) (- x y)) (map length l1) (map length l2))))

(match-lengths? '( () (1 2) (3 4 5)) '( (1) (2 3 4) (5 6 7 8))); -> #t,
(match-lengths? '( () (1 2) (3 4 5)) '( () (2 3 4) (5 6 7))); -> #f


(or (null? (car m)) (null? (cdr m)))

'(())


(define (triangular? m)
  (define (allZero col) (all? (lambda (x) (= x 0)) col))
  (if (or (null? (car m)) (null? (cdr m))) ; за да обработва коректно и квадратни, и правоъгълни матрици
      #t      ; първата колона, без първия ред
      (and (allZero (cdr (map car m)))
           (triangular? (cdr (map cdr m))))))

(define (null-mat? m)
  (or (null? m) (null? (car m))))

