(define (accumulate null-value combine term next a b)
  (if (> a b) null-value
  (combine (term a) (accumulate null-value combine term next (next a) b))))

(define (identity x) x)
(define (add1 x) (+ x 1))
(define (fact-accum n)
  (accumulate 1 * identity add1 1 n)
  )

(define (expt-accum x n)
  (accumulate 1 * (lambda (a) x) add1 1 n)
  )

(define (count-divisors n a b)
  (define (divider? x)
    (if (= (remainder n x) 0) 1
        0))
  (accumulate 0 + divider? add1 a b)
  )

(define (powers-sum x n)
  (accumulate 0 + (lambda (a) (* a (expt-accum x a)))  add1 1 n)
  )

;(powers-sum 2 3)

(define (combinations n k)
  ( / (fact-accum n) (* (fact-accum k) (fact-accum (- n k))))) 
 
(define (prime-accum n)
  (= 1 (count-divisors n 1 (- n 1))))

(define (accum-iter op nv a b term next)
  (define (helper counter result)
    (if (> counter b) result
        (helper (next counter) (op result (term counter)))))
  (helper a nv))

(define (!! n)
  (define (term x) (if (= 0 (remainder (- n x) 2)) x 1))
  (accum-iter * 1 1 n term add1)
  )

(!! 5) ;-> 15    ; =1*3*5
(!! 10) ;-> 3840 ; =2*4*6*8*10

(define (derivative f)
  (define delta 1e-3)
  (lambda (x) (/ (- (f (+ x delta)) (f x)) delta)))

(define (constantly c)
  (lambda (x) c))
(define forever-21 (constantly 21))
(forever-21 5); -> 21
(forever-21 10) ;-> 21
((constantly 21) 10); -> 21

(define (flip f)
  (lambda (x y) (f y x)))

(define f (flip -))
(f 4 10); -> 6          ; = (- 10 4)
((flip f) 4 10); -> -6

(define (compose f g)
  (lambda (x) (f (g x)))
  )

(define (++ x) (+ x 1))
(define (-- x) (- x 1))
(define +- (compose ++ --))
(+- 5) ;-> 5 ; = (++ (-- 5))

(define (complement p)
  (lambda (x) (not (p x))))

(define (less-than-5? x) (< x 5))
(define f (complement less-than-5?))
(f 3) ; => #f
(f 5) ; => #t
(f 7) ; => #t


(define (comb2 n k)
  (define (term i) (/ (- (+ n 1) i) i))
  (define (term2 i) (/ (- (+ n i) k) i))
  (accum-iter * 1 1 k term add1))


; (accum-iter op nv a b term next)
(define (prime2? n)
  (define (divider? x) (= 0 (remainder n x)))
  (accum-iter (lambda (x y) (and x (not y))) #t 2 (- n 1) divider? add1))



