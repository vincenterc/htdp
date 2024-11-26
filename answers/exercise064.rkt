#lang htdp/bsl

; computes the distance of ap to the origin
; measures the Manhattan distance of ap to the origin
(check-expect (manhattan-distance (make-posn 0 5)) 5)
(check-expect (manhattan-distance (make-posn 7 0)) 7)
(check-expect (manhattan-distance (make-posn 3 4)) 7)
(check-expect (manhattan-distance (make-posn 8 6)) 14)
(check-expect (manhattan-distance (make-posn 5 12)) 17)
(define (manhattan-distance ap)
  (+ (abs (- (posn-x ap) 0))
     (abs (- (posn-y ap) 0))))
