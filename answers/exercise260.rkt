#lang htdp/isl

(define list-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                     12 11 10 9 8 7 6 5 4 3 2 1))
(define list-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                     17 18 19 20 21 22 23 24 25))

; Nelon -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf.v2 (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

; (inf.v2 list-1)
; (inf.v2 list-2)
