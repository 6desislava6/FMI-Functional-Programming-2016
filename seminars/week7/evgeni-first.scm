#lang scheme
;; Map
(define (any? pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (any? pred (cdr lst)))))

(define (my-map fn . lsts)
  (define (my-map-1 fn lst) ; the simple map variant
    (if (null? lst)
        '()
        (cons (fn (car lst))
              (my-map-1 fn (cdr lst)))))

  (if (or (null? lsts)
          (any? null? lsts))
      '()
      (let ((heads (my-map-1 car lsts))
            (tails (my-map-1 cdr lsts)))
        (cons (apply fn heads)
              (apply my-map fn tails)))))


(define (depth lst)
  (cond
    ((not (list? lst)) 0)
    ((null? lst) 1)
    (else (+ 1 (list-max (map depth lst))))))

(define (list-max lst)
  (foldl max (car lst) lst))

(define (flatten lst)
  (cond
    ((not (list? lst)) (list lst))
    ((null? lst) '())
    (else (append (flatten (car lst)) (flatten (cdr lst))))))

(define (index-of val lst)
  (define (helper counter remaining)
    (cond
      ((null? remaining) -1)
      ((= val (car remaining)) (+ 1 counter))
      (else (helper (+ 1 counter) (cdr remaining)))))
  (helper 0 lst))
      

(define (funky? lst)
  (define (sum l) (foldl + 0 l))
  (define (filter-row row ll) (filter (lambda (x) (not (equal? x row))) ll))
  (define (sum-all ll) (map sum ll))
    (foldl (lambda (row result)
           (foldl (lambda (column result) (or result (not (= -1 (index-of column (sum-all (filter-row row lst)))))))
                  #f row)
    ) #f lst)
    )


(funky? '((0 1 -1)
          (1 1)
          (12 3)))

(define (get-column i m)
  (map (lambda (x) (list-ref  x i)) m))

(get-column 1 '((1 2 3)
             (4 5 6)))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ 1 a) b))))
(define (transpose m)
  (map (lambda (i) (get-column i m)) (from-to 0 (length m))))

;(transpose '((1 2 3)
 ;            (4 5 6)))

(define (map-with-index f l)
  (define zipped (map cons l (from-to 0 (- (length l) 1))))
  (map (lambda (x i) (f x i)) zipped))

;(define (main-diag m)
 ; (map-with-index (lambda (x i) (list-ref x i)) m))
;(main-diag '((1 2) (3 4) (5 6))) ; => (1 4)
;(main-diag '((1 2 3) (4 5 6))) ; => (1 5)