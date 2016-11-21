#lang racket

(define (solve-interval x1 x2 f g)
  (let*
      ((delta (expt 10 -6))
       (delta-direction (if (> (minus f g x1) (minus f g x2)) delta (- 0 delta))))
    (define (helper current)
      (if (< (- (f current) (g current)) delta) current
          (helper (+ current delta-direction))))
    (helper 0)))

;(solve (lambda (x) (- (* x x x))) (lambda (x) (- x 2)))

(define (minus f g x)
  (- (f x) (g x)))

(define (in-interval? x1 x2 f g)
    (negative? (* (minus f g x1) (minus f g x2))))

(define (find-interval f g x1 x2 delta-dir)
      (cond
        ((in-interval? x1 x2 f g) (solve-interval x1 x2 f g))
        ((positive? delta-dir) (find-interval f g (max x1 x2) (+ delta-dir (max x1 x2)) delta-dir))
        (else (find-interval f g (min x1 x2) (+ delta-dir (min x1 x2)) delta-dir))))