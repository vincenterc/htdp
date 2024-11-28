#lang htdp/bsl

; Posn Number -> Posn
; updates the x component of p with n
(check-expect (posn-up-x (make-posn 10 0) 13) (make-posn 13 0))
(define (posn-up-x p n)
  (make-posn n (posn-y p)))
