#lang htdp/isl+

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous
; (2) (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in
; one of the two halves, picks according to (2)
(check-within (find-root poly 0 3) 2 0.0001)
(check-within (find-root poly 3 6) 4 0.0001)
; step  left  f-left  right  f-right    mid     f-mid
;  n=1     3      -1   6.00     8.00   4.50      1.25
;  n=2     3      -1   4.50     1.25   3.75   -0.4375
(define (find-root f left right)
  0)
