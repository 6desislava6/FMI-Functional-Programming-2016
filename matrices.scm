(define (all p? l) (foldr (lambda (x y) (and (p? x) y)) #t l))

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))
       (all list? m)
       (all (lambda (row) (= (length row)
                             (length (car m)))) m)))

(define get-rows length)
(define (get-columns m) (length (car m)))

(define get-first-row car)
(define (get-first-column m) (map car m))

(define del-first-row cdr)
(define (del-first-column m) (map cdr m))

(define (get-row i m) (list-ref m i))
(define (get-column i m)
  (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (accumulate cons '() 0 (- (get-columns m) 1)
              (lambda (i) (get-column i m)) 1+))

(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrices m1 m2) (map sum-vectors m1 m2))

(define (mult-vectors v1 v2) (apply + (map * v1 v2)))
(define (mult-matrices m1 m2)
  (let ((m2t (transpose m2)))
    (map (lambda (row)
           (map (lambda (column) (mult-vectors row column))
                m2t))
         m1)))

(define (tree? t)
  (or (null? t)
      (and (list t) (= (length t) 3))
      (tree? (cadr t))
      (tree? (caddr t))))