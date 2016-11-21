(define (map2 f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map f (cdr lst)))))

(define (filter pred? lst)
  (cond ((null? lst) lst)
        ((pred? (car lst)) (cons (car lst) (filter pred? (cdr lst))))
        (else (filter pred? (cdr lst)))))

(define (length2 lst)
  (if (null? lst) 0
      (+ 1 (length2 (cdr lst)))))

(define (reverse2 lst)
  (define (helper current result)
    (if (null? current) result
        (helper (cdr current) (cons (car current) result))))
  (helper lst '()))

(define (nth n lst)
  (cond
    ((null? lst) "грешка")
    ((= n 0) (car lst))
    (else (nth (- n 1) (cdr lst)))))

(define (range from to)
  (if (> from to) '()
      (cons from (range (+ 1 from) to))))

(define (digit-list n)
  (define (helper current result)
    (if (= 0 current) result
        (helper (quotient current 10) (cons (remainder current 10) result))))
  (helper n '()))

(define (take n lst)
  (cond
    ((null? lst) '())
    ((= n 0) '())
    (else (cons (car lst) (take (- n 1) (cdr lst))))))

;(take 3 (range 1 5)) ;-> '(1 2 3)
;(take 10 (range 1 5)) ; -> '(1 2 3 4 5)

(define (drop n lst)
  (cond
    ((null? lst) '())
    ((= n 0) lst)
    (else (drop (- n 1) (cdr lst)))))

;(drop 3 (range 1 5)) ;-> '(4 5)
;(drop 10 (range 1 5)) ; -> '()

(define (chunk n lst)
  (define (helper current counter result-small result)
    (cond
      ((null? current) (append result (list result-small)))
      ((= 0 counter) (helper current n '() (append result (list result-small))))
      (else (helper (cdr current) (- counter 1) (append result-small (list  (car current))) result))))
  (helper lst n '() '()))

(chunk 4 (range 1 10))



(define (all p? lst)
  (if (null? lst) #t
  (and (p? (car lst)) (all p? (cdr lst)))))

;(all even? '(1 2 3 4 5)); -> #f


;foldl
(define (reduce op term lst nv)
  (define (helper current result)
    (if (null? current) result
        (helper (cdr current) (op (car current) result))))
  (helper (map term lst) nv))

(define (rev2 lst)
  (reduce cons (lambda (x) x) lst '()))

(define (all2 p? lst)
  (reduce (lambda (c r) (and c r)) p? lst #t))

;(all2 even? '(2 2 2 2 2)); -> #f

(define (any? p? lst)
  (reduce (lambda (c r) (or c r)) p? lst #f))

;(any? even? '(1 2 3 4 5)); -> #t
;(any? (lambda (x) (> x 10)) '(4 2 6 3 1)); -> #f


(define (zip lst1 lst2)
  (if
    (or (null? lst1) (null? lst2)) '()
   (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))))

;(zip '(1 2 3 4) '(#t #f #f))

(define (remove-all val lst)
  (filter (lambda (x) (not (= x val))) lst))

(remove-all 1 '(2 1 3 2 4 1 2 3))

(define (remove-first val lst)
  (cond
    ((null? lst) lst)
    ((= val (car lst)) (cdr lst))
    (else (cons (car lst) (remove-first val (cdr lst))))))

;(reduce op term lst nv)
(define (sum lst)
  (reduce + (lambda (x) x) lst 0))

(define (sum-of-sums lst)
  (map2 sum lst))
 ;(sum-of-sums '((1 2 3) (0) (5 4 2 7)))



(define (group-pairs lst)
  (define firsts (filter (lambda (x) (equal? (cdr x) (cdar lst))) lst))
  (define rest (filter (lambda (x) (not (equal? (cdr x) (cdar lst)))) lst))
  (if (null? lst) '()
      (cons firsts (group-pairs rest))))

(define (combine-groups lst)
  (map (lambda (l) (list (cdar l) (map car l))) lst)) 

(define (group-by f lst)
  (combine-groups (group-pairs (map (lambda (x) (cons x (f x))) lst))))