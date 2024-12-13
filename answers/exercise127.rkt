#lang htdp/bsl

(define-struct ball [x y speed-x speed-y])

; (number? (make-ball 1 2 3 4)) == #false

; (ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3)) == 3

; (ball-y (make-ball (+ 1 2) (+ 3 3) 2 3)) == 6

; (ball-x (make-posn 1 2)) -> error

; (ball-speed-y 5) -> error
