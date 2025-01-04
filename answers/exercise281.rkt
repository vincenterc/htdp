#lang htdp/isl+

(require 2htdp/image)

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

(define RED-DOT (circle 5 "solid" "red"))

(lambda (n) (< n 10))

(lambda (n1 n2) (number->string (* n1 n2)))

(lambda (n) (if (even? n) 0 1))

(lambda (ir1 ir2) (> (IR-price ir1) (IR-price ir2)))

(lambda (p img) (place-image RED-DOT (posn-x p) (posn-y p) img))
