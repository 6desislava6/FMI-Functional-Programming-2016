#lang scheme

; важни: assoc, remove, remq

(define g '((1 2 3 4)
            (2 3)
            (3 1)
            (4)))
(assoc 1 g)
(assoc 4 g)

(define (add-node g node)
  (if (assoc node g)
      g
      (cons (list node) g)))

(define (get-nodes g)
  (map car g))

(define (remove-node g v)
  (let ((kv (assoc v g)))
    (map (lambda (row) (remove v row)) (remq kv g))))

;associative list -> alist
; първото срещане го заменяме със стойността v

;; Update
; Макрос, който подава g на 2 пъти на add-node и накрая на update-fn
(define (update alst k v)
  (update-fn alst k (lambda (x) v)))

(define (update-fn alst k f)
  (cond ((null? alst) '())
        ((equal? k (caar alst))
          (cons (cons k (f (cdar alst)))
                (cdr alst)))
        (else
          (cons (car alst)
                (update-fn (cdr alst) k f)))))
  ; намира стойността v, който е асоциирана с k: alist[k] = v
  ; използва v1 = fn(v) и alist[k] = fn(v)



(define (add-edge g v1 v2)
 ;Ако върховете ги няма, ще ги добавим и тях!
 ;леко императивен стил, по който добавяме двата върха, преди да правим каквото и да е
  (let* ((g (add-node g v1)) 
        (g (add-node g v2)))
    (update-fn g v1 (lambda (l) (cons v2 l)))))

(define (remove-edge g v1 v2)
  (update-fn g v1 (lambda (l) (remove v2 l))))

(define (get-edges g)
  (apply append (map (lambda (l) (map (lambda (other) (cons (car l) other)) (cdr l))) g)))
    
    
; частични функции
(define (partial fn . args)
  (lambda (rest-args)
    (apply fn (append args rest-args))))


(define (1+ . numbers)
  ((partial + 1) numbers))

; TODO - ламбдата винаги си приема произволен брой?

;!!! partial-right
;(filter (partial > 10) l)


