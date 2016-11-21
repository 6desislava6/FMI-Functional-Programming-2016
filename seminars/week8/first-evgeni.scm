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