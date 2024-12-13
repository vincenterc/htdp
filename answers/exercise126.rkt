#lang htdp/bsl

(define-struct point [x y z])
(define-struct none  [])

; (make-point 1 2 3)

; (make-point (make-point 1 2 3) 4 5)

; (make-point (+ 1 2) 3 4)
; ==
; (make-point 3 3 4)

; (make-none)

; (make-point (point-x (make-point 1 2 3)) 4 5)
; ==
; (make-point 1 4 5)