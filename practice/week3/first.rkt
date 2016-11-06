(define (prod init a b)
  (define (prod-helper result counter)
    (if (= counter b) (* result b)
        (prod-helper (* result counter) (+ 1 counter)))
  )
  (if (> a b) init
      (prod-helper init a)
  )
 )
(define (count-digits n)
  (cond
    ((= 0 n) 0)
    ((= (remainder n 10) 0) (+ 1 (log10 n)))
     ((ceiling (log10 n)))))

(define (reverse-int n)
  (define (reverse-helper current result)
    (if (= current 0) result
        (reverse-helper (quotient current 10) (+ (* result 10) (remainder current 10)))
    )
  )
  (reverse-helper n 0)
  )

(define (palindrome? n)
  (equal? (number->string n) (number->string (reverse-int n)))
  )

(define (circle-area r)
  (let ((pi 3.14))
    (* pi r r)
  )
 )

(define (s a b c)
  (let ((p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (- p c))))
  )