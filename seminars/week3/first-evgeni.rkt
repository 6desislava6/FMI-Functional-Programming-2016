(define (f a b c)
  (define (sum-sqr a b) (+ (* a a) (* b b)))
  (cond
    ((and (> a b) (> b c)) (sum-sqr a b))
    ((and (> a b) (> c b)) (sum-sqr a c))
    ((> a c) (sum-sqr a b))
    (else  (sum-sqr c b))
 )
)

; a + |b|
(define (m a b)
  ((if (> b 0) + -) a b))


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause))
  )

(define (rec x)
  (new-if (= x 0) 0 (/ 1 x)))

