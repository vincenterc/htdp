#lang htdp/isl

; Nelon -> Number
; determines the largest number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local ((define largest-in-rest (sup (rest l))))
       (if (> (first l) largest-in-rest)
           (first l)
           largest-in-rest))]))

(sup (list 2 1 3))
