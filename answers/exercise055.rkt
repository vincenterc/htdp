#lang htdp/bsl

(require 2htdp/image)

(define HEIGHT 300) ; distances in pixels
(define WIDTH  100)

(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))

(define CENTER (/ (image-height ROCKET) 2))

(define (place-rocket height)
  (place-image ROCKET
               10 (- height CENTER)
               BACKG))