(define (sum x)
  (if (= 0 x) 0
      (+ x (sum (- x 1))) 
  ))

(define (pow x n)
  (if (= n 1) x
      (* x (pow x (- n 1))))
  )

(define (sqr2 x) (* x x))
(define (fast-pow x n)
  (define k (quotient n 2))
  (cond
    ((= n 1) x)
    ((= (remainder n 2) 0) (sqr2 (fast-pow x (/ n 2))))
    (else (* x (sqr2 (fast-pow x (quotient n 2)))))
  ))

(define (fact-iter n)
  (define (fact-helper current result)
    (if (= 1 current) result
      (fact-helper (- current 1) (* result current))
      )
    )
  (fact-helper n 1)
  )

(define (sum-range a b)
  (define (sum-helper current result)
    (if (= current b) (+ result b)
        (sum-helper (+ current 1) (+ result current))
    )
  )
  (sum-helper a 0)
  )

(define (reverse-digits x)
  (define (reverse-helper current reversed)
    (if (= 0 current) reversed
        (reverse-helper (quotient current 10) (+ (remainder current 10) (* 10 reversed))))
  )
  (reverse-helper x 0))

(define (fib-iter n)
  (define (fib-helper current previous counter)
    (if (= counter n) current
        (fib-helper (+ current previous) current (+ counter 1))))
    (if (< n 3) 1
        (fib-helper 1 1 2))
  )

(define (is-prime? x)
  (define (is-prime-helper current)
    (cond
      ((> current (ceiling (sqrt x))) #t)
      ((= 0 (remainder x current)) #f)
      (else (is-prime-helper (+ 1 current)))
      ))
  (if (= 2 x) #t
      (is-prime-helper 2))
        
  )
(is-prime? 7)