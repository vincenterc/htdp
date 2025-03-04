#lang htdp/bsl

(define (f x y)
  (+ (* 3 x) (* y y)))

; (+ (f 1 2) (f 2 1))
; ==
; (+ (+ (* 3 1) (* 2 2)) (f 2 1))
; ==
; (+ (+ 3 (* 2 2)) (f 2 1))
; ==
; (+ (+ 3 4) (f 2 1))
; ==
; (+ 7 (f 2 1))
; ==
; (+ 7 (+ (* 3 2) (* 1 1)))
; ==
; (+ 7 (+ 6 (* 1 1)))
; ==
; (+ 7 (+ 6 1))
; ==
; (+ 7 7)
; ==
; 14

; (f 1 (* 2 3))
; ==
; (f 1 6)
; ==
; (+ (* 3 1) (* 6 6))
; ==
; (+ 3 (* 6 6))
; ==
; (+ 3 36)
; ==
; 39

; (f (f 1 (* 2 3)) 19)
; ==
; (f (f 1 6) 19)
; ==
; (f (+ (* 3 1) (* 6 6)) 19)
; ==
; (f (+ 3 (* 6 6)) 19)
; ==
; (f (+ 3 36) 19)
; ==
; (f 39 19)
; ==
; (+ (* 3 39) (* 19 19))
; ==
; (+ (* 3 39) (* 19 19))
; ==
; (+ 117 (* 19 19))
; ==
; (+ 117 361)
; ==
; 478
