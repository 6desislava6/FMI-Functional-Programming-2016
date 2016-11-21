; задача 12
(define (foldl op nv lst)
  (if (null? lst) nv
      (foldl op (op nv (car lst)) (cdr lst))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (insert var l)
  (if (or (null? l) (< var (car l)))
      (cons var l)
      (cons (car l) (insert var (cdr l)))))

(define (insertion-sort l)
  (if (null? l) l
      (foldr insert '() l)))


; задача 13
(define (compose . fns)
  (if (null? fns) (lambda (x) x)
      (lambda (x) ((car fns) ((apply compose (cdr fns)) x)))))

;(define f )
;(compose (lambda (x) (* 2 x)) (lambda (x) (* x x))  (lambda (x) (+ 1 x)))
;((compose (lambda (x) (* 2 x)) (lambda (x) (* x x))  (lambda (x) (+ 1 x))) 1)

(define (reverse2 lst)
  (foldr cons '() lst))

(define (flip fn)
  (lambda xs (apply fn (reverse xs))))

;(define list^ (flip list))

(define  (histogram lst)
  (distinct (map (lambda (x) (cons x (occurrences lst x))) lst)))

(define (occurrences lst val)
  (foldl (lambda (r x) (if (= val x) (+ 1 r) r)) 0 lst))

;(occurrences '(1 6 1 6 1 6) 1)

(define (index-of lst val)
  (define (helper current index)
    (cond ((null? current) -1)
          ((equal? (car current) val) index)
          (else (helper (cdr current) (+ 1 index)))))
  (helper lst 0))

(define (distinct lst)
  (if (null? lst) '()
        (if (= -1 (index-of (cdr lst) (car lst)))
            (distinct (cdr lst))
            (cons (car lst) (distinct (cdr lst))))))
(histogram '(1 4 3 5 1 2 3 4 1))