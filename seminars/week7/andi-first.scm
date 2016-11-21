#lang scheme
(define (accumulate op nv term a b next)
  (if (> a b) nv
      (op (term a) (accumulate op nv term (next a) b next))))

(define (meetTwice? f g a b)
  (accumulate + 0 (lambda (x) (if (= (f x) (g x)) 1 0)) a b (lambda (x) (+ 1 x))))

(meetTwice? (lambda(x)x) (lambda(x) (- x)) -3 1)

(define (range a b)
  (accumulate cons '() (lambda (x) x) a b (lambda (x) (+ 1 x))))

(define (meetTwice?? f g a b)
  (< 2 (length (filter (lambda (x) (= (f x) (g x))) (range a b)))))

(meetTwice?? (lambda(x)x) (lambda(x) (- x)) -3 1)

;-------------------------

(define (is-duplicate? x l)
  (> (length (filter (lambda (i) (equal? i x)) l))  1))

(define (find-duplicates lst)
  (filter (lambda (x) (is-duplicate? x lst)) lst))

(define (remove-all x lst)
  (filter (lambda (i) (not (equal? x i))) lst))

(define (distinct lst)
  (if (null? lst) lst
      (cons (car lst) (distinct (remove-all (car lst) (cdr lst))))))
(distinct '( 1 2 3 1 10 2 3 3 3))

(define (max-list lst)
  (apply max lst))

(apply append '(1 1) '(10 10) '(100 100) '())

(define (maxDuplicate lst)
  (let ((temp (apply append (map find-duplicates lst))))
    (if (null? temp) #f
        (apply max temp))))

(maxDuplicate '((1 2 3 2) '(-­4 -­4) (5)))

;-------------
(define (all? p? lst)
  (cond ((null? lst) #t)
        ((not (p? (car lst))) #f)
        (else (all? p? (cdr lst)))))

(define (any? p? lst)
  (not (all? (lambda (x) (not (p? x))) lst)))

(define (contains-multiple k lst)
  (any? (lambda (x) (= (remainder x k) 0)) lst))

(define (checkMatrix? m k)
  (all? (lambda (row) (contains-multiple k row)) m))


(checkMatrix? '((1 2 6) (3 8 9) (10 12 11)) 3) ;→ #t

(checkMatrix? '((1 2 4) (3 8 9) (10 12 11)) 3) ;→ #f

;----------------------

