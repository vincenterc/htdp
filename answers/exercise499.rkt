#lang htdp/isl+

; [List-of Number] -> Number
; computes the product of the numbers on alon0
(check-expect (product '()) 1)
(check-expect (product '(1 2)) 2)
(check-expect (product '(1 2 3)) 6)
(define (product alon0)
  (local (; [List-of Number] -> Number
          ; computes the product of the numbers on alon
          ; accumulator a is the product of the numbers
          ; that alon lacks from alon0
          (define (product/a alon a)
            (cond
              [(empty? alon) a]
              [else (product/a (rest alon)
                               (* (first alon) a))])))
    (product/a alon0 1)))

; The performance of product/a is still O(n).
