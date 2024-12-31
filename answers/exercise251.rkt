#lang htdp/isl

; [List-of Number] -> Number
; computes the sum of
; the numbers on l
(check-expect (sum '()) 0)
(check-expect (sum (list 1)) 1)
(check-expect (sum (list 2 1)) 3)
(check-expect (sum (list 3 2 1)) 6)
; (define (sum l)
;   (cond
;     [(empty? l) 0]
;     [else
;      (+ (first l)
;         (sum (rest l)))]))
(define (sum l)
  (fold1 l + 0))


; [List-of Number] -> Number
; computes the product of
; the numbers on l
(check-expect (product '()) 1)
(check-expect (product (list 1)) 1)
(check-expect (product (list 2 1)) 2)
(check-expect (product (list 3 2 1)) 6)
; (define (product l)
;   (cond
;     [(empty? l) 1]
;     [else
;      (* (first l)
;         (product (rest l)))]))
(define (product l)
  (fold1 l * 1))

(define (fold1 l f initial)
  (cond [(empty? l) initial]
        [else
         (f (first l)
            (fold1 (rest l) f initial))]))
