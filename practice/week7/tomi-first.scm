#lang scheme
(define M '((1 2 3) (4 5 6) (7 8 9)))

(define get-rows-count length)

(define (get-cols-count m)
  (if (null? m) 0
      (length (car m))))

(define get-row list-ref)

(define (get-col m i)
  (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (if (or (null? m) (null? (car m))) '()
      (cons (map car m) (transpose (map cdr m)))))

;(transpose M)

(define (delete-row m i)
  (cond
    ((null? m) '())
    ((= i 0) (cdr m))
    (else (cons (car m) (delete-row (cdr m) (- i 1))))))


(define (transpose2 m)
  (define (helper i)
    (if
     (= i (get-cols-count m)) '()
     (cons (get-col m i) (helper (+ i 1)))))
  (helper 0))

(define (delete-col m i)
  (transpose (delete-row (transpose m) i)))


;(delete-col M 0)

(define (delete-at l i)
  (define (delete-helper idx l)
    (cond
      ((null? l) l)
      ((= i idx) (cdr l))
      (else (cons (car l) (delete-helper (+ 1 idx) l)))))
  (delete-helper i l))

(define (main-diag m)
  (if
   (or (null? m) (null? (car m))) '()
   (cons (caar m) (main-diag (delete-row (delete-col m 0) 0)))))

(main-diag M)

(define (get-at m i j)
  (list-ref (list-ref m i) j))

(define (second-diag m)
  (define (helper i)
    (if (or (= i (get-rows-count m)) (= i (get-cols-count m))) '()
        (cons (get-at m i (- (get-cols-count m) (+ 1 i))) (helper (+ i 1)))))
  (helper 0))

(second-diag M)

(define (sum-diag m)
  (apply + (append (main-diag m) (second-diag m))))

(sum-diag M)

(define T '(5
            (3
             (1 () ())
             (4 () ()))
            (10
             (7 () ())
             (8
              ()
              (9 () ())))))

(define empty-tree? null?)

(define (make-tree root left right)
  (list root left right))

(define (root t)
  (if (empty-tree? t) '()
      (car t)))
(define (left-tree t)
  (if (empty-tree? t) '()
      (cadr t)))

(define (right-tree t)
  (if (empty-tree? t) '()
      (caddr t)))



                 
                
               