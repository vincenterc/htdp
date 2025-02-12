#lang htdp/isl+

; [List-of X] -> Number
; determines how many items are on alox0
(check-expect (how-many '()) 0)
(check-expect (how-many '(1)) 1)
(check-expect (how-many '(a b)) 2)
(define (how-many alox0)
  (local (; [List-of X] Number -> Number
          ; determines how many items are on alox
          ; accumulator a is the numbers of items
          ; that alon lacks from alon0
          (define (how-many/a alox a)
            (cond
              [(empty? alox) a]
              [else (how-many/a (rest alox) (add1 a))])))
    (how-many/a alox0 0)))

; The performance of how-many/a is still O(n).

; The accumulator does reduce the amount of space
; needed to compute the result.
