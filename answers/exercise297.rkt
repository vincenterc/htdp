#lang htdp/isl+

; Number Number Posn -> Number
; computes the distance between the points (x, y) and p
(check-expect (distance-between 0 0 (make-posn 3 4)) 5)
(check-expect (distance-between 2 5 (make-posn 10 11)) 10)
(check-expect (distance-between -3 2 (make-posn -8 14)) 13)
(define (distance-between x y p)
  (sqrt (+ (sqr (- (posn-x p) x))
           (sqr (- (posn-y p) y)))))
