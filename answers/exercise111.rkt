#lang htdp/bsl

(define-struct vec [x y])
; A Vec is
;   (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector

(error "make-vec: positive number expected")

; Any -> Vec
; generates a Vec given x and y,
; if x and y are positive numbers
(define (checked-make-vec x y)
  (cond
    [(and (number? x) (> x 0) (number? y) (> y 0)) (make-vec x y)]
    [else (error "make-vec: positive numbers expected")]))
