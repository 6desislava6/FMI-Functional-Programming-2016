;function gcd(a, b)
;    while a ≠ b
;        if a > b
    ;    a := a − b
      ;  else
     ;      b := b − a
    ;return a

(define (gcd2 a b)
  (define (gcd-helper first second)
    (cond
      ((= first second) first)
      ((> first second) (gcd-helper (- first second) second))
      (else (gcd-helper first (- second first)))
      )
    )
  (gcd-helper a b)
  )

(define (lcm2 a b)
  (/ (* a b) (gcd2 a b))
  )

(define (helper first second)
    (cond
      ((or (= 0 first) (= 0 second)) #t)
      ((= (remainder first 10) (remainder second 10)) (helper (quotient first 10)  (quotient second 10)))
      (else #f)
    )
 
(define (ends-with? a b)
  
  )
  (helper a b))

(define (substr? a b)
  (
  )