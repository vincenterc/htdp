#lang htdp/bsl

; computes the distance of ap to the origin
(check-expect (distance-to-0 (make-posn 0 5)) 5)
(check-expect (distance-to-0 (make-posn 7 0)) 7)
(check-expect (distance-to-0 (make-posn 3 4)) 5)
(check-expect (distance-to-0 (make-posn 8 6)) 10)
(check-expect (distance-to-0 (make-posn 5 12)) 13)
(define (distance-to-0 ap)
  (sqrt
   (+ (sqr (posn-x ap))
      (sqr (posn-y ap)))))

; Evaluate
; (distance-to-0 (make-posn 3 4))
; (distance-to-0 (make-posn 6 (* 2 4)))
; (+ (distance-to-0 (make-posn 12 5)) 10)
