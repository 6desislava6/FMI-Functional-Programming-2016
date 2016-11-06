#lang racket
(define (if2 t x y)
  (or (and t x) y)
)
(define (and2 x y)
  (if x y #f))

(define (or2 x y)
  (if x #t y))

(define (not2 x)
  (if x #f #t))

(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))
  )
)

(define (fib2 n)
    (define (inner-fib current previous count)
      (if (= count n) (+ current previous)
          (inner-fib (+ current previous) current (+ count 1))
          )
      )
  (cond
    [(negative? n) "nope"]
    [(< n 2) n]
    [else (inner-fib 1 1 3)]
    )
  )

(define (solve a b c)
  (let ((d (- (* b b) (* (* 4) (* a c)))))
    (cond
      [(= a 0)
       (cond
         [(and (= b 0) (= c 0)) "every x"]
         [(and (= b 0) (not (= c 0))) "no solution"]
         [else (/ (- c) b)]
           ) ]
      [(> d 0) (cons (/ (- (sqrt d) b) 2) (/ (- (- (sqrt d)) b) 2))]
      [(= d 0) (/ (- b) 2)]
      [(< d  0) "no solution"]
     )
   )
  )

(define (nchoosek n k)
  (if (or (= k 0) (= k n)) 1
  (+ (nchoosek (- n 1) k) (nchoosek (- n 1) (- k 1)))
  )
)

(define (fact n)
  (if (< n 2) 1
      (* n (fact (- n 1))))
  )


(define (nchoosek2 n k)
  (/ (fact n) (* (fact (- n k)) (fact k)))
)

(define (exp2 x n)
  (if (= n 0) 1
      (if (= (remainder n 2) 0)
          (sqr (exp2 x (/ n 2)))
          (* (sqr (exp2 x (/ (- n 1) 2))) x)
          )
   )
  )

(define (fast-exp x n)
  (define (sq x) (* x x))
  (define k (quotient n 2))
  (cond
    [(zero? n) 1]
    [(even? n) (sq (fast-exp x k))]
    [else (* (sq (fast-exp x k)) x)] 
    )
  )

(define (fast-exp2 x n)
  (cond
    [(and (positive? n) (integer? n)) (fast-exp x n)]
   ; тук е за отрицателно цяло число
    [(integer? n) (/ 1 (fast-exp x (- n)))]
    ; значи не е цяло
    [ else (* (fast-exp2 x (round n))
              (expt x (- n (round n))))]
           
    )
  )
(fast-exp2 2 1/2)