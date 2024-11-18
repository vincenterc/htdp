#lang htdp/bsl

(define (distance x y) (sqrt (+ (sqr x ) (sqr y))))

(= (distance 3 4) 5)
(= (distance 12 5) 13)
