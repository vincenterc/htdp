#lang htdp/isl+

; [X] [X X -> Boolean] [NEList-of X] -> Boolean
; determines whether l is sorted according to cmp
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
(define (sorted? cmp l)
  (cond [(empty? (rest l)) #true]
        [else (and (cmp (first l) (second l))
                   (sorted? cmp (rest l)))]))
