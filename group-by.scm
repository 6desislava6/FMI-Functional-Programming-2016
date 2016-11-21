; Зад.16. За ентусиастите
; 1. '(1 2 3 4 5)
; -> '((1 . #f) (2 . #t) (3 . #f) (4 . #t) (5 . #f))
(define (makePairs f lst)
  (map (lambda (x) (cons x (f x))) lst))

; 2. '((1 . #f) (2 . #t) (3 . #f) (4 . #t) (5 . #f))
; -> '( ((1 . #f) (3 . #f) (5 . #f))
;       ((2 . #t) (4 . #t)) )
(define (groupPairs lst)
  (let* ((predicate (lambda (x) (equal? (cdr (car lst)) (cdr x))))
         (firsts (filter predicate lst))
         (rest (filter (lambda (x) (not (predicate x))) lst)))
    (if (null? lst)
        '()
        (cons firsts (groupPairs rest)))))

; 3. '( ((1 . #f) (3 . #f) (5 . #f))
;       ((2 . #t) (4 . #t)) )
; -> '((#f (1 3 5)) (#t (2 4)))
(define (combineGroups lst)
  (map (lambda (l) (list (cdar l) (map car l))) lst))

(define (group-by-f f lst)
  (combineGroups (groupPairs (makePairs f lst))))
; haskell: group-by-f f = combineGroups . groupPairs . makePairs f

; iei
(define (group-by-f* f lst)
  (let* ((predicate (lambda (x) (equal? (f x) (f (car lst)))))
         (firsts (filter predicate lst))
         (rest (filter (lambda (x) (not (predicate x))) lst)))
    (if (null? lst)
        '()
        (cons (list (f (car lst)) firsts)
              (group-by-f* f rest)))))