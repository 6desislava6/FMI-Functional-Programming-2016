(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y)
  )
;(test 0 (p))

(define (sum n)
  (define (sum-helper current sum)
    (if (> current n) sum
        (sum-helper (inc current) (+ current sum)))
  )
  (sum-helper 1 0))

(define (fact n)
  (define (fact-helper current prod)
    (if (> current n) prod
        (fact-helper (+ 1 current) (* prod current))
    )    
  )
  (fact-helper 1 1))

(define (fib n)
  (define (fib-helper previous current counter)
    (if (= n counter) current
        (fib-helper current (+ current previous) (+ 1 counter)))
  )
  (if (< n 3) 1
      (fib-helper 1 1 2)
      ))