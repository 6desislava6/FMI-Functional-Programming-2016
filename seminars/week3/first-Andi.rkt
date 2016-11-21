(define (reverse-int n)
  (define (reverse-helper current result)
    (if (= current 0) result
        (reverse-helper (quotient current 10) (+ (* result 10) (remainder current 10)))
    )
  )
  (reverse-helper n 0)
  )

; брой цифри в число

(define (log10 n) (/ (log n) (log 10)))
(define (count-digits n)
  (cond
    ((= 0 n) 0)
    ((= (remainder n 10) 0) (+ 1 (log10 n)))
     ((ceiling (log10 n)))))

(define (palindrome? n)
  (equal? (number->string n) (number->string (reverse-int n)))
  )

(define (divisors-sum n)
  (define (divisors-helper counter result)
    (cond
      ((> counter n) result)
      ((= 0 (remainder n counter)) (divisors-helper (+ 1 counter) (+ result counter)))
      (else (divisors-helper (+ 1 counter) result))
      ))
  (divisors-helper 1 0))

(define (perfect? n)
  (= (divisors-sum n) (* 2 n))
  )

; да не се преизчислява всеки път n!!!

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

(define (increasing? n)
  (define (increasing-helper current number)
    (cond
      ((= current 0) #t)
      ((<= number (remainder current 10)) #f)
      (else (increasing-helper (quotient current 10) (remainder current 10)))
     )
    )
  (increasing-helper (quotient n 10) (remainder n 10))
  )
;(increasing? 445)
; имплементацията на Андрей - имаме bit, който следи на кой бит сме.

(define (helper current result base new-base)
  (if (= 0 current) result
      (helper (quotient current new-base) (+ (* result base) (remainder current new-base)) base new-base)
      )
  )
(define (to-binary n)
  (quotient (helper n 1 10 2) 10)
  )

(to-binary 8)
(to-binary 15)
(to-binary 42)

(define (to-decimal n)
  (quotient (helper n 1 2 10) 2)
  )

(to-decimal 101010)
(to-decimal (to-binary 1234))

(define (toBinary n)
  (define (helper n res bit)
    (if (= n 0)
        res
        (helper (quotient n 2) (+ res (* (remainder n 2) (expt 10 bit))) (+ bit 1))))
  (helper n 0 0))
