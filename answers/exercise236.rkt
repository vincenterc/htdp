#lang htdp/isl

; Number Lon -> Lon
; adds n to each item on l
(define (add n l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (+ (first l) n)
      (add n (rest l)))]))

; Lon -> Lon
; adds 1 to each item on l
(check-expect (add1* '()) '())
(check-expect (add1* '(1)) '(2))
(check-expect (add1* '(1 2)) '(2 3))
; (define (add1* l)
;   (cond
;     [(empty? l) '()]
;     [else
;      (cons
;       (add1 (first l))
;       (add1* (rest l)))]))
(define (add1* l)
  (add 1 l))

; Lon -> Lon
; adds 5 to each item on l
(check-expect (plus5 '()) '())
(check-expect (plus5 '(1)) '(6))
(check-expect (plus5 '(1 2)) '(6 7))
; (define (plus5 l)
;   (cond
;     [(empty? l) '()]
;     [else
;      (cons
;       (+ (first l) 5)
;       (plus5 (rest l)))]))
(define (plus5 l)
  (add 5 l))

; Lon -> Lon
; subtracts 2 from each item on l
(check-expect (sub2 '()) '())
(check-expect (sub2 '(5)) '(3))
(check-expect (sub2 '(5 10)) '(3 8))
(define (sub2 l)
  (cond [(empty? l) '()]
        [else (cons (- (first l) 2)
                    (sub2 (rest l)))]))
; (define (sub2 l)
;   (add (- 2) l))
