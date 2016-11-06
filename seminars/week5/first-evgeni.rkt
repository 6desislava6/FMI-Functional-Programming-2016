;дефиниция на дърво: <r, T1, T2> където T1, T2 са дървета
; null - празен списък
; <v, l> - списък, където v - value, l - list (т.е. друг списък)я
;(let 
;    ((l (cons 1 (cons 2 (cons 3 '())))))
;  (car (cdr (cdr l))))
; empty?
; equal?

(define l (cons 1 (cons 2 (cons 3 '()))))

(define (len l)
  (if (empty? 0) 0
      (+ 1 (len (cdr l)))))

(define (empty? l) (equal? l '()))

(define (nth n lst)
  (if (empty? lst) #f
      (if (= n 0) (car lst)
          (nth (- n 1) (cdr lst)))))

(define (append2 l1 l2)
  (if (empty? l1) l2
      (cons (car l1) (append2 (cdr l1) l2))))

;(list 1 2 3 4)
;'(1 2 3)

(define (reverse2 lst)
  (define (helper current result)
    (if (empty? lst) '()
        (helper (cdr lst) (cons (car current) result))))
  (helper lst '()))

(define (reverse3 lst)
  (if (empty? lst) '()
      (append (reverse3 (cdr lst)) (list (car lst)))))

(define (reverse-iter lst result)
  (if (empty? lst) result
      (reverse-iter (cdr lst) (append (list (car lst)) result))))
 
 
 ;(reverse-iter l '())

(define (map2 f lst)
  (cons (f (car lst)) (map f (cdr lst))))

(define (square x) (* x x))
;(map2 square (list 1 2 3)) ; => (1 4 9)

(define (filter f lst)
  (if (empty? lst) '()
      (if (f (car lst)) (cons (car lst) (filter f (cdr lst)))
          (filter f (cdr lst)))))

(define (fold f val lst)
  (if (empty? lst) val
      (f (car lst) (fold f val (cdr lst)))))
(define (fold1 f lst) (fold f 0 lst))
;(fold + 0 (list 1 2 3)) ; => 6
;(fold1 + (list 1 2 3)) ; => 6
;(fold1 + (list 1)) ; => 1
;(fold1 + '()) ; => #f

(define (index-of lst val)
  (define (helper current index)
    (cond ((empty? current) -1)
          ((= (car current) val) index)
          (else (helper (cdr current) (+ 1 index)))))
  (helper lst 0))
                               
;(index-of (list 0 1 2 3 1) 3) ; => 1
(define (last-index-of lst val)
  (- (- (length lst) 1) (index-of (reverse3 lst) val)))
; 1 2 3 4
;(last-index-of (list 0 1 2 3 1) 1) ; => 4

(define (distinct? lst)
  (if (empty? lst) #t
        (and (= -1 (index-of (cdr lst) (car lst))) (distinct? (cdr lst)))))

;(distinct? (list 1 2 3 4)) ; => #t
;(distinct? (list 1 2 3 2)) ; => #f

(define (distinct lst)
  (define (helper current result)
    (cond ((empty? current) result)
        ((> (index-of result (car current)) -1) (helper (cdr current) result))
        (else (helper (cdr current) (append result (list (car current)))))))
  (helper lst '()))
  
(distinct (list 1 2 3 2)) ; => (1 2 3)


(define (every? f lst)
  (fold (lambda (x y) (and x y)) #t (map f lst)))
(every? odd? (list 1 2 3)) ; => #f
(every? odd? (list 1 5 3)) ; => #t

(define (any? f lst)
  (fold (lambda (x y) (or x y)) #f (map f lst)))
(any? even? (list 1 2 3)) ; => #t
(any? even? (list 1 5 3)) ; => #f

(define (range n)
  (if (= n 0) '()
      (append (range (- n 1)) (list (- n 1)))))
 (range 5) ; => (0 1 2 3 4)

(define (take n lst)
  (if (= n 0) '()
      (cons (car lst) (take (- n 1) (cdr lst)))))
(take 5 (range 100)) ; => (0 1 2 3 4)

; http://zsurface.com/image/fold.svg
(define (fold-right op val lst)
  (if (empty? val)
      (op (car lst) (fold-right op val (cdr lst)))))
(define (fold-left op val lst)
  (define (helper result current)
    (if (empty? current) result
        (helper (op (car current) result) (cdr current))))
  (helper val lst))

; не е ok
;(define (reverse4 lst)
  ;(fold-right (lambda (x y) (cons y x)) '() lst))

(define (drop n lst)
  (filter (lambda (x) (<= n (index-of lst x))) lst)) ; => (20 21 22)
(drop 20 (range 23)) ; => (20 21 22)

(define (take2 n lst)
  (filter (lambda (x) (> n (index-of lst x))) lst));

(define (remove lst val)
  (filter (lambda (x) (not (= x val))) lst))
(remove (list 1 2 1 3 1 4) 1) ; => (2 3 4)

; До задача 12
