#lang racket
(define (all? p? lst)
  (cond
    ((null? lst) #t)
    ((not (p? (car lst))) #f)
    (else (all? p? (cdr lst)))))

(define (any? p? lst)
  (foldr (lambda (x r) (or (p? x) r)) #f lst))

(define (sum-row lst)
  (apply + lst))

(define (drop-nth lst n)
  (if (= n 0) lst
      (cons (car lst) (drop-nth (cdr lst) (- n 1)))))

(define (range a b)
  (if (> a b) '()
      (cons a (range (+ 1 a) b))))

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

