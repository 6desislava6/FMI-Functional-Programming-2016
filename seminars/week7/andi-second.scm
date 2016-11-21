#lang racket
(define (sublist? lst1 lst2)
  (cond
    ((null? lst2) (null? lst1))
    ((begins-with? lst1 lst2) #t)
    (else (begins-with? lst1 (cdr lst2)))))

(define (begins-with? lst1 lst2)
  (equal? lst1 (take-n (length lst1) lst2)))

(define (take-n n lst)
  (if (or (= n 0) (null? lst)) '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

(define (begins-with2 lst1 lst2)
  (cond
    ((and (null? lst1) (null? lst2)) #t)
    ((null? lst1) #t)
    ((null? lst2) #f)
    ((= (car lst1) (car lst2)) (begins-with2 (cdr lst1) (cdr lst2)))
    (else #f)))

(define (make-set lst)
  (if (null? lst) '()
      (cons (car lst) (make-set (remove-all (car lst) (cdr lst))))))

(define (remove-all x lst)
  (filter (lambda (i) (not (equal? x i))) lst))


(sublist? '(2 3 4) '(1 2 3 4 5)); -> #t
(sublist? '(2 3 5) '(1 2 3 4 5)); -> #f ; тъй като търсените елементи не са последователни
(sublist? '(4 3 5) '(1 2 3 4 5)); -> #f
(sublist? '() '()); -> #t ;(!)



(make-set '(1 1 10 10 100 1 10))

(define (quick-sort lst)
  (if (null? lst) '()
      (append (quick-sort (filter (lambda (x) (< x (car lst))) lst))
              (filter (lambda (x) (= x (car lst))) lst)
              (quick-sort (filter (lambda (x) (> x (car lst))) lst)))))

(define (histogram lst)
  (define (count x lst) (length (filter (lambda (i) (equal? i x)) lst)))
  (make-set (map (lambda (x) (cons x (count x lst))) lst)))

(define (nth-row n m)
  (list-ref m n))

(define matrix '((1 2 3) (4 5 6)))

(define (nth-column n m)
  (map (lambda (row) (list-ref row n)) m))

(define (row-count m) (length m))
(define (row-column m) (length (car m)))

(define (transpose m)
  (if (null? (car m)) '()
      (cons (map car m) (transpose (map cdr m)))))

; махаме първия стълб map cdr m
; взимаме първия стълб map car m
;хитрооо

(define (range a b)
  (if (> a b) '()
      (cons a (range (+ 1 a) b))))

(define (replicate val times)
  (if (= times 0) '()
      (cons val (replicate val (- times 1)))))

(define (all? p? lst)
  (cond
    ((null? lst) #t)
    ((not (p? (car lst))) #f)
    (else (all? p? (cdr lst)))))

(define (triangular? m)
  (let ((indexed (map cons m (range 0 (- (length m) 1)))))
    (all? (lambda (x) x) (map (lambda (row) (begins-with? (replicate 0 (cdr row)) (car row))) indexed))))

(triangular? matrix)

(triangular? '((1 2 3 4 100)
               (0 5 6 7 0)
               (0 0 8 9 12)
               (0 0 0 1 152))); -> #t


(define (replicate-list lst times)
  (apply append (map (lambda (el) (replicate el times)) lst)))

(replicate-list '(a b) 3)

(define (descartes lst1 lst2)
  (map cons (cycle lst1 (length lst2)) (replicate-list lst2 (length lst1))))

 (define (cycle lst times)
   (apply append (replicate lst times)))

(descartes '(1 2 3) '(a b)) 