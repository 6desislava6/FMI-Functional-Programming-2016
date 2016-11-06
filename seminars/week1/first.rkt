(define (solve a b c)
  (let ((d (- (* b b) (* (* 4) (* a c)))))
    (cond
      ((= a 0)
       (cond
         ((and (= b 0) (= c 0)) "every x")
         ((and (= b 0) (not (= c 0))) "no solution")
         (else (/ (- c) b))
           ) )
      ((> d 0) (cons (/ (- (sqrt d) b) 2) (/ (- (- (sqrt d)) b) 2)))
      ((= d 0) (/ (- b) 2))
      ((< d  0) "no solution")
     )
   )
  )

(let ((x 3)
      (y 4))
      (= x y))

(let ((x (+ 1 2))) (+ x x))
((lambda (x) (+ x x)) (+ 1 2))
