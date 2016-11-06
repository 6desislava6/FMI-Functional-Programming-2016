(define (add3) (lambda (x) (+ x 3)))
(define add3 (lambda (x) (+ x 3)))

(define (flip f)
  (lambda (x y) (f y x)))

(define (curry f)
  (lambda (x) (lambda (y) (lambda (z) (f x y z))))
  )

(define (curry-binary f)
  (lambda (x) (lambda (y) (f x y))))

(define (add1)
  ((curry-binary +) 3))

(define (prod term next a b)
  (if (> a b)
      1
   (* (term a) (prod term next (next a) b))))

(define (prod-odd a b)
  (prod (lambda (x) (if (odd? x) x 1)) (lambda (x) (+ x 1)) a b))

(define (accumulate op nv term a next b)
  (if (> a b) nv
      (op (term a) (accumulate op nv term (next a) next b))))

(define (prod-odd-squared a b)
  (accumulate * 1 (lambda (x) (if (odd? x) (* x x) 1)) a (lambda (x) (+ 1 x)) b))

(define (acc-iter op nv term a next b)
  (define (helper current result)
    (if (> current b) result
        (helper (next current) (op (term current) result))))
  (helper a nv))

(define (prod-odd-squared2 a b)
  (acc-iter * 1 (lambda (x) (if (odd? x) (* x x) 1)) a (lambda (x) (+ 1 x)) b))

(define (id x) x)
(define (1+ x) (+ x 1))
(define (power x n)
  (acc-iter * 1 (lambda (k) x) 1 1+ n))

(define (n!! n)
  (define (term x) (if (= 0 (remainder (- n x) 2)) x 1))
  (accumulate * 1 term 1 1+ n))