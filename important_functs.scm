(define (id x) x)
(define (1+ x) (+ 1 x))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

; в nv се натрупва резултата
(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (sum2 a b)
  (accumulate + 0 a b id 1+))

(define (sum a b term next)
  (accumulate + 0 a b term next))

(define (product a b term next)
  (accumulate * 1 a b term next))

; compose
(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0) id (compose f (repeated f (- n 1)))))

(define (repeated f n)
  (accumulate compose id 1 n (lambda (i) f) 1+))

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (derive-n f n dx)
  ((accumulate compose id 1 n
               (lambda (i) (lambda (f) (derive f dx))) 1+) f))

;((derive-n (lambda (x) (* x x x)) 2 0.00001) 6)


(define (lcons x y) (lambda (p) (p x y)))
(define (lcar z) (z (lambda (x y) x)))
(define (lcdr z) (z (lambda (x y) y)))

(define (lcons x y) (lambda (p) (if p x y)))
(define (lcar z) (z #t))
(define (lcdr z) (z #f))

(define (map-i f lst)
  (define (helper current result)
    (if (null? current) result
        (helper (cdr current) (append result (list (f (car current)))))))
  (helper lst '()))

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (map2 func lst)
	(foldr (lambda (x y) (cons (func x) y)) '() lst))

(define (filter2 pred lst)
  (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))


(define (deep-reverse l)
  (cond ((null? l) '())
        ((atom? l) l)
        (else (append (deep-reverse (cdr l))
                      (list (deep-reverse (car l)))))))

(define (flatten l)
  (cond ((null? l) '())
        ((atom? l) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

(define (deep-fold nv term op l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-fold nv term op (car l))
                  (deep-fold nv term op (cdr l))))))

(define (atom? x) (not (pair? x)))
(define (rcons x l) (append l (list x)))
(define (deep-reverse l) (deep-fold '() id rcons l))

;(deep-reverse '(1 (2 3) (5 6)))
;
;(append '(1 2 3 4) (list '(5 6 7 877777)))
;(append '(1 2 3 4) '(5 6 7 877777))

(define (deep-fold2 nv term op l)
  (foldr op nv
         (map (branch valid-atom?
                      term
                      (lambda (l) (deep-fold nv term op l))
                      l))))

(map foldr (list * +) '(1 0) '((1 2 3) (4 5 6)))

(define (all p? l) (foldr (lambda (x y) (and (p? x) y)) #t l))

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))
       (all list? m)
       (all (lambda (row) (= (length row)
                             (length (car m)))) m)))



(define (branch p? f g) (lambda (x) (if (p? x) (f x) (g x))))

(define (valid-atom? x) (and (not (pair? x)) (not (null? x))))

(define (deep-fold nv term op l)
  (foldr nv op (map (branch valid-atom? term (lambda (x) (deep-fold nv term op x))) l)))


(define (evali x) (eval x (interaction-environment)))