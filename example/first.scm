(define (meetTwice? f g a b)
  (define (helper counter current)
    (cond
      ((= counter 0) #t)
      ((> current b) #f)
      ((= (f current) (g current)) (helper (- counter 1) (+ 1 current)))
      (else (helper counter (+ 1 counter)))))
  (helper 2 a))

(define (foldr nv op lst)
  (if (null? lst) nv
      (op (car lst) (foldr nv op (cdr lst)))))

(define (filter p? lst)
  (foldr '() (lambda (x r) (if (p? x) (cons x r) r)) lst))

(define (occurrences val lst)
  (cond
    ((null? lst) 0)
    ((= val (car lst)) (+ 1 (occurrences val (cdr lst))))
    (else (occurrences val (cdr lst)))))

(define (duplicates lst)
  (filter (lambda (x) (>= (occurrences x lst) 2)) lst))

(define (max-duplicates-inner lst)
  (if
    (null? (duplicates lst)) #f
    (foldr (car (duplicates lst)) max (duplicates lst))))

(define (maxDuplicate ll)
  (map max-duplicates-inner ll))
