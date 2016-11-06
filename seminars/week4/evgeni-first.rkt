(define (identity x) x)
(define (add1 x) (+ 1 x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-integers a b)
  (sum identity a add1 b))

(define (sum-cubes a b)
  (sum (lambda (x) (* x x x)) a add1 b))

(define (sum-e a b)
  (sum (lambda (x) (/ 1 (fact x))) a add1 b))

; problem 3:

(define (integral f a b delta)
  (let ((n (- (/ (- b a) delta) 1)))
  (sum (lambda (x) (* (f (+ a (* (/ (+ (* 2 x) 1) 2) delta))) delta)) 0 add1 n)))

;(integral (lambda (x) x) 1 2 1e-3)

; problem 4:
(define (sum2 term a next b)
  (define (iter current result)
    (if (> current b) 
  	result 
  	(iter term (next current) next (+ result (term current)))))
  (iter a 0))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum3 term a next b)
  (accumulate + 0 term a next b)
  )
(define (sum-integers2 a b)
  (sum3 identity a add1 b))

(define (prod term a next b)
  (accumulate * 1 term a next b)
  )
(define (fact n)
  (prod identity 1 add1 n)
  )



(define (prime? x)
  (define (sqn) (ceiling (sqrt x)))
  (define (prime-helper current)
    (cond
      ((> current (sqn)) #t)
      ((= 0 (remainder x current)) #f)
      (else (prime-helper (+ 1 current)))
      ))
  (if (= 2 x) #t
      (prime-helper 2))
        
  )
; (filtered-accumulate combiner null-value pred term a next b)

(define (filtered-accumulate combiner null-value pred term a next b)
  (cond ((> a b) null-value)
        ((not (pred a)) (combiner 0 (filtered-accumulate combiner null-value pred term (next a) next b)))
      (else (combiner (term a) (filtered-accumulate combiner null-value pred term (next a) next b)))))

(define (sum-of-prime-squares n)
  (filtered-accumulate + 0  prime? (lambda (x) (* x x)) 1 add1 n)
  )

(define (derivative f)
  (let ((delta 1e-3))
    (lambda (x) (/ (- (f (+ x delta)) (f x)) delta))
    )
  )

((derivative (lambda (x) (* x x))) 1)
(define (constantly n) (lambda (x) n))
(define forever-21 (constantly 21))

(define (flip f) (lambda (x y) (f y x)))
(define cons^ (flip cons))
(cons^ 2 3) ; => (3 . 2)

(define (curry-3 f) (lambda (x) (lambda (y) (lambda (z) (f x y z)))))
(define +. (curry-3 +))
(((+. 1) 2) 3) ; => 6

(define (compose f g) (lambda (x) (f (g x))))
(define f (compose add1 (lambda (x) (* x x)))) ; x^2 + 1
(f 4) ; => 17
(f 7) ; => 50

(define (complement p) (lambda (x) (not (p x))))
(define (less-than-5? x) (< x 5))
(define f (complement less-than-5?))
(f 3) ; => #f
(f 5) ; => #t
(f 7) ; => #t

(define (iterate n f)
  (define (helper counter resulting-f)
    (if (> counter n) resulting-f
        (helper (+ 1 counter) (compose f resulting-f)))
  )
  (helper 1 identity)
  )
(define plus-10 (iterate 10 add1))
(plus-10 1) ; => 11
(plus-10 25) ; => 35

(define pow-8 (iterate 3 (lambda (x) (* x x))))
(pow-8 1) ; => 1
(pow-8 2) ; => 256
(define (nth-derivative n f) (iterate n derivative))
;((nth-derivative 2 (lambda (x) (* x x x))) 2)

(define (cons2 a b)
  (lambda (z)
    (if z a b)))


(define p (cons2 2 3))
(define (car2 pair)
  (pair #t)
  )
(define (cdr2 pair)
  (pair #f)
  )
(define p (cons2 2 3))
(car2 p) ; => 2
(cdr2 p) ; => 3


(define (left p)
  (p (lambda (a b) a)))
(define (right p)
  (p (lambda (a b) b)))
