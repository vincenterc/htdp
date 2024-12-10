#lang htdp/bsl

(require 2htdp/image)

(define WIDTH 200)
(define HALF-WIDTH (/ WIDTH 2))
(define HEIGHT 200)
(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

(define UFO
  (overlay (circle 8 "solid" "green")
           (rectangle 50 4 "solid" "green")))
(define UFO-HEIGHT (image-height UFO))
(define HALF-UFO-HEIGHT (/ UFO-HEIGHT 2))
(define X-UFO-START HALF-WIDTH)
(define Y-UFO-START HALF-UFO-HEIGHT)

(define TANK (rectangle 50 15 "solid" "blue"))
(define TANK-WIDTH (image-width TANK))
(define HALF-TANK-WIDTH (/ TANK-WIDTH 2))
(define TANK-HEIGHT (image-height TANK))
(define HALF-TANK-HEIGHT (/ TANK-HEIGHT 2))
(define Y-TANK (- HEIGHT HALF-TANK-HEIGHT))
(define X-TANK-START HALF-TANK-WIDTH)

(define MISSILE (triangle 15 "solid" "black"))
