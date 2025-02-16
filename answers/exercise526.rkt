#lang htdp/isl+

(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels

; Number -> Posn
; determines the point on the circle with CENTER
; and RADIUS whose angle is (* factor (* 2 pi))
(check-within (circle-pt 0) (make-posn 400 200) 0.001)
(check-within (circle-pt 90/360) (make-posn 200 400) 0.001)
(check-within (circle-pt 180/360) (make-posn 0 200) 0.001)
(check-within (circle-pt 270/360) (make-posn 200 0) 0.001)
(check-within (circle-pt 360) (make-posn 400 200) 0.001)
(define (circle-pt factor)
  (make-posn (+ (* RADIUS (cos (* factor (* 2 pi)))) (posn-x CENTER))
             (+ (* RADIUS (sin (* factor (* 2 pi)))) (posn-y CENTER))))
