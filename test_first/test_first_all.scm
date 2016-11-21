#lang racket
(define (module x)
  (if (negative? x) (- x) x))

(define (solve f g)
  (let*
      ((delta (expt 10 -6))
       ; дали да ходим наляво, или надясно
       (delta-direction (if (> (- (f 0) (g 0)) (- (f 1) (g 1))) delta (- delta))))
    (define (helper current)
      (if (< (module (- (f current) (g current))) delta) current
          (helper (+ current delta-direction))))
    (helper 0)))

;(solve (lambda (x) (- (* x x x))) (lambda (x) (- x 2)))
(solve (lambda (x) (* x x x)) (lambda (x) (+ x 0.001)))
;-----------------------
; втора задача
(define (range a b)
  (if (> a b) '()
      (cons a (range (+ 1 a) b))))

(define (all? p? lst)
  (cond
    ((null? lst) #t)
    ((not (p? (car lst))) #f)
    (else (all? p? (cdr lst)))))

(define (any? p? lst)
  (foldr (lambda (x r) (or (p? x) r)) #f lst))

(define (all-with-index p? lst)
  (all? (lambda (el-index) (p? (car el-index) (cdr el-index))) (map cons lst (range 0 (- (length lst) 1)))))

(define (drop-nth lst n)
  (if (= n 0) lst
      (cons (car lst) (drop-nth (cdr lst) (- n 1)))))

(define (sum-row lst)
  (apply + lst))

(define (checksum ll)
  (let
      ((sums (map sum-row ll)))
    (all-with-index
     (lambda (row index)
       (any? (lambda (el) (not (not (member el (drop-nth sums index))))) row))
     ll)))


(checksum '((1 1) (2 3) (-4 5)))
(checksum '((1 2) (5) (7 10)))

;---------------------
; трета задача
(define (transpose m)
  (if (or (null? m) (null? (car m)))
      '()
      (cons (map car m) (transpose (map cdr m)))))

(define (all-increasing lst)
  (cond
    ; нямаме елементи
    ((null? lst) #t)
    ; имаме само един елемент
    ((null? (cdr lst)) #t)
    ((< (car lst) (cadr lst)) (all-increasing (cdr lst)))
    (else #f)))

(define (exists-increasing? ll)
  (any? (lambda (x) x) (map all-increasing (transpose ll))))

(define matrix '((1 8 3) (10 9 6) (7 2 9)))
(define matrix2 '((7 8 9) (1 9 3) (4 5 6)))

;----------------
; четвърта задача
(define (remove-all val lst)
  (filter (lambda (el) (not (equal? val el))) lst))

(define (make-set lst)
  (if
   (null? lst) '()
   (cons (car lst) (make-set (remove-all (car lst) lst)))))

(define (distinct-all? lst)
  (equal? lst (make-set lst)))

; друга версия
(define (distinct-all2? lst)
  (cond
    ((null? lst) #t)
    ((member (car lst) (cdr lst)) #f)
    (else (distinct-all2? (cdr lst)))))

(define (make-pairs lst)
  ; няма как да му подадем нечетно, заради проверка в главната функция
  (if
   (null? lst) '()
   (cons (cons (car lst) (cadr lst)) (make-pairs (cddr lst)))))

(define (replicate val times)
  (if (= times 0) '()
      (cons val (replicate val (- times 1)))))

(define (prev-look-and-say lst)
  (cond
   ((not (= 0 (remainder (length lst) 2))) #f)
   ((not (distinct-all? (map cdr (make-pairs lst)))) #f)
   (else (apply append (map (lambda (el-occ) (replicate (cdr el-occ) (car el-occ))) (make-pairs lst))))))

(define l3 '(1 1 2 3 1 2))
(prev-look-and-say l3)
(prev-look-and-say '(1 1 5 1 1 2))
(prev-look-and-say '(1 1 5 1 1 2 6))

;-------------
; Второ решение на втора задача
(define (exists? val lst)
  (not (not (member val lst))))

(define (checksum2 ll)
  (let
      ((sums (map sum-row ll))
       (indexed (map cons ll (range 1 (length ll)))))
    (all? (lambda (x) x) (any? (lambda (x) x) (map
     (lambda (row-index)
       (map (lambda (el) (exists? el (drop-nth sums (cdr row-index)))) (car row-index)))
     indexed)))))

(checksum2 '((1 1) (2 3) (-1 2)))
(checksum2 '((1 2) (5) (7 10)))
   