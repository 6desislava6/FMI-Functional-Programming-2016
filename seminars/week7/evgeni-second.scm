#lang scheme
(define (any? pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (any? pred (cdr lst)))))


(define (drop-nth n l)
  (cond
    ((null? l) l)
    ((= n 0) (cdr l))
    (else (cons (car l) (drop-nth (- n 1) (cdr l))))))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ 1 a) b))))

(define (any-with-index? p? l)
  (any?
   (lambda (pair) (p? (car pair) (cdr pair)))
   (map cons l (from-to 0 (- (length l) 1)))))

(define (get-column i m)
  (map (lambda (x) (list-ref  x i)) m))

(define (main-diag m)
  (map
   (lambda (x) (list-ref (list-ref m x) x))
   (range (min (length m) (length (car m))))))

(main-diag '((1 2) (3 4) (5 6))) ; => (1 4)
(main-diag '((1 2 3) (4 5 6))) ; => (1 5) 

(define (sum l) (foldl + 0 l))
(define (funky? lst)
  (let ((sums (map sum lst)))
    (any-with-index?
     (lambda (l i)
       (any?
        (lambda (x)
          (member x (drop-nth i sums)))
        l))
     lst)))

;(funky? '((0 1 -1)
;          (1 1)
 ;         (2 3)))

;a1*delta11-a2*delta12+a3*delta13

                
(define (drop-col i m)
  (map (lambda (x) (drop-nth i x)) m))

(define (det m)
  (if (= 1 (length (car m))) (caar m)
      (sum
       (map
        (lambda (x i) (* x (expt -1 i) (det (drop-col i m))))
        (car m) (from-to 1 (length (car m)))))))


;(det
 ; '((1 0 2 -1)
  ;  (3 0 0 5)
   ; (2 1 4 -3)
    ;(1 0 5 0))) ; => 30
