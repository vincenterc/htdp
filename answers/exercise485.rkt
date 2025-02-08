#lang htdp/isl+

; A Tree is one of:
; - Number
; - (list Tree Tree)

; Tree -> Number
(check-expect (sum-tree 1) 1)
(check-expect (sum-tree (list 1 (list 2 3))) 6)
(define (sum-tree t)
  (cond [(number? t) t]
        [else (+ (sum-tree (first t))
                 (sum-tree (second t)))]))

; on the order of 2^l where l is the length of the tree

; The size of the tree is the length of the tree

; The worst possible shape likes a pyramid
; having 2^l numbers.

; The best possible shape has only one number
