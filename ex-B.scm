#lang scheme
(define (mixed? f g a b)
  (define (helper bigger smaller current)
    (cond
      ((and bigger smaller) #t)
      ((> current b) #f)
      ((< (f current) (g current)) (helper bigger #t (+ 1 current)))
      ((> (f current) (g current)) (helper #t smaller (+ 1 current)))
      (else (helper bigger smaller (+ 1 current)))))
  (helper #f #f a))

;---------------
;(mixed? (lambda (x) x) (lambda (x) (- x)) -3 1)


(define (filter p? lst)
  (foldr '() (lambda (x r) (if (p? x) (cons x r) r)) lst))

(define (occurrences val lst)
  (cond
    ((null? lst) 0)
    ((= val (car lst)) (+ 1 (occurrences val (cdr lst))))
    (else (occurrences val (cdr lst)))))

(define (not-duplicates lst)
  (filter (lambda (x) (= (occurrences x lst) 1)) lst))

(define (max-not-duplicates-inner lst)
  (if
    (null? (not-duplicates lst)) #f
    (foldr (car (not-duplicates lst)) max (not-duplicates lst))))

(define (any? p? lst)
  (foldr #f (lambda (x r) (or (p? x) r)) lst))

(define (maxUnique ll)
  (let ((not-dupls (filter number? (map max-not-duplicates-inner ll))))
    (if (null? not-dupls) #f
        (foldr (car not-dupls) max not-dupls))))

;(maxUnique '((1 2 3 2) (5 5) (0)))
 ;(maxUnique '((1 2 1 2) (5 5) ()))
;---------------

(define (dividable-all lst k)
  (if (null? lst) #t
      (and (= 0 (remainder k (car lst))) (dividable-all (cdr lst) k))))

(define (checkMatrix? m k)
  (foldr #t (lambda (x r) (and (not (dividable-all x k)) r)) m))

;(checkMatrix? '((1 2 6) (3 8 9) (6 11 12)) 12)
;(checkMatrix? '((1 2 7) (3 8 9) (6 11 12)) 12)
;-------------
(define (get-last lst)
  (if (null? (cdr lst)) (car lst)
      (get-last (cdr lst))))

(define (chunk-ascending lst)
  (define (helper remaining current result)
    (cond
      ((null? remaining) (append result (list current)))
      ((> (car remaining) (get-last current)) (helper (cdr remaining) (append current (list (car remaining))) result))
      (else (helper (cdr remaining) (list (car remaining)) (append result (list current))))))
  (if (null? lst) '()
      (helper lst (list (car lst)) '())))
  
(define (longestAscending­ lst)
  (let* ((chunked (chunk-ascending lst))
         (lengths (map (lambda (x) (cons (length x) x)) chunked)))
    ;(foldr (car chunked) (lambda (x r) (if (> (car x) (car r)) x r)) lengths)))
    ; Защо не е точно обратното??? >= 
    (cdr (foldl (lambda (x r) (if (>= (car x) (car r)) x r)) (car lengths)  lengths))))
    ;lengths))
    
;(longestAscending­ '(5 3 8 6 4 2 6 7 1))
(longestAscending­ '(6 5 4 3 2 1))
