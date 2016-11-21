(define (meetTwice? f g a b)
  (define (helper counter current)
    (cond
      ((= counter 0) #t)
      ((> current b) #f)
      ((= (f current) (g current)) (helper (- counter 1) (+ 1 current)))
      (else (helper counter (+ 1 current)))))
  (helper 2 a))

;---------------

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

(define (any? p? lst)
  (foldr #f (lambda (x r) (or (p? x) r)) lst))

(define (maxDuplicate ll)
  (let ((dupls (filter number? (map max-duplicates-inner ll))))
    (if (null? dupls) #f
        (foldr (car dupls) max dupls))))

(maxDuplicate '((1 2 3 2) (-4 -4) (5)))
;---------------

(define (dividable lst k)
  (if (null? lst) #f
      (or (= 0 (remainder (car lst) k)) (dividable (cdr lst) k))))
(define (checkMatrix? m k)
  (foldr #t (lambda (x r) (and (dividable x k) r)) m))
;-------------
(define (get-last lst)
  (if (null? (cdr lst)) (car lst)
      (get-last (cdr lst))))

(define (chunk-descending lst)
  (define (helper remaining current result)
    (cond
      ((null? remaining) (append result (list current)))
      ((< (car remaining) (get-last current)) (helper (cdr remaining) (append current (list (car remaining))) result))
      (else (helper (cdr remaining) (list (car remaining)) (append result (list current))))))
  (if (null? lst) '()
      (helper lst (list (car lst)) '())))
  
(define (longestDescending­ lst)
  (let* ((chunked (chunk-descending lst))
         (lengths (map (lambda (x) (cons (length x) x)) chunked)))
    ;(foldr (car chunked) (lambda (x r) (if (> (car x) (car r)) x r)) lengths))
    (cdr (foldr (car lengths) (lambda (x r) (if (>= (car x) (car r)) x r)) lengths))))
    ;lengths
    
(longestDescending­ '(5 3 8 6 4 2 6 7 1 10 9 8 7)) 