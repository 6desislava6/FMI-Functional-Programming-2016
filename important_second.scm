;Зад.5. Взимане/премахване на няколко елементи от списък
(define (take* n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst)
            (take* (- n 1) (cdr lst)))))

(define (drop* n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop* (- n 1) (cdr lst))))

; Зад.6. Разцепване на списък на парчета
(define (chunk n lst)
  (if (null? lst)
      '()
      (cons (take* n lst)
            (chunk n (drop* n lst)))))

; Зад.8. zip
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

;Зад.12. Да се напише функция (insert val lst), която вмъква стойността val на правилното място в сортирания списък lst:
; Зад.12.
(define (insert val lst)
  (if (or (null? lst) (< val (car lst)))
      (cons val lst)
      (cons (car lst) (insert val (cdr lst)))))

; Зад.13.
(define (insertion-sort lst)
  (if (null? lst)
      '()
      (insert (car lst) (insertion-sort (cdr lst)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (insertion-sort* lst) ; за ентусиастите
  (foldr insert '() lst))

; Зад.15. Композиция на произволен брой функции
(define (compose . fns)
  (if (null? fns)
      (lambda (x) x)
      (lambda (x) ((car fns) ((apply compose (cdr fns)) x)))))