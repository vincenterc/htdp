#lang htdp/bsl+

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; An NEList-of-numbers is one of:
; - (cons Number '())
; - (cons Number NEList-of-number)
; interpretation non-empty lists of numbers

; NEList-of-numbers -> Boolean
; determines if the numbers in l are sorted in descending order
(check-expect (sorted>? (cons 2 '())) #true)
(check-expect (sorted>? (cons 1 (cons 2 '()))) #false)
(check-expect (sorted>? (cons 3 (cons 2 '()))) #true)
(check-expect (sorted>? (cons 0 (cons 3 (cons 2 '())))) #false)
(define (sorted>? l)
  (cond
    [(empty? (rest l)) #true]
    [else (and (>= (first l) (first (rest l)))
               (sorted>? (rest l)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-expect (sort> '()) '())
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))
(check-satisfied (sort> '(3 2 1)) sorted>?)
(check-satisfied (sort> '(1 2 3)) sorted>?)
(check-satisfied (sort> '(12 20 -5)) sorted>?)
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l
(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5))
              (list 20 12 -5))
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
; (check-expect (sort>/bad (list 3 1 2)) (list 3 2 1))
; Can not use check-satisfied to formulate this test case
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))
