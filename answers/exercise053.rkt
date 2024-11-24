#lang htdp/bsl

(require 2htdp/image)

; An LR (short for launching rocket) is one of:
; – "resting"
; – NonnegativeNumber
; interpretation "resting" represents a grounded rocket
; a number denotes the height of a rocket in flight

(define HEIGHT 300) ; distances in pixels
(define WIDTH  100)

(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))

(place-image ROCKET (/ WIDTH 2) HEIGHT BACKG)

(place-image ROCKET (/ WIDTH 2) (/ HEIGHT 2) BACKG)

(place-image ROCKET (/ WIDTH 2) 0 BACKG)