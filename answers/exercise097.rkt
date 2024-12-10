#lang htdp/bsl

(require 2htdp/image)

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is a Posn.
; interpretation (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of:
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a
; space invader game

(define WIDTH 200)
(define HEIGHT 200)
(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

(define UFO
  (overlay (circle 8 "solid" "green")
           (rectangle 50 4 "solid" "green")))

(define TANK (rectangle 50 15 "solid" "blue"))
(define TANK-HEIGHT (image-height TANK))
(define HALF-TANK-HEIGHT (/ TANK-HEIGHT 2))
(define Y-TANK (- HEIGHT HALF-TANK-HEIGHT))

(define MISSILE (triangle 15 "solid" "black"))

; Tank Image -> Image
; adds t to the given image im
(check-expect (tank-render (make-tank 28 -3) BACKGROUND)
              (place-image TANK 28 Y-TANK BACKGROUND))
(check-expect (tank-render (make-tank 100 3) BACKGROUND)
              (place-image TANK 100 Y-TANK BACKGROUND))
(define (tank-render t im)
  (place-image
   TANK
   (tank-loc t) Y-TANK
   im))

; UFO Image -> Image
; adds u to the given image im
(check-expect (ufo-render (make-posn 10 20) BACKGROUND)
              (place-image UFO 10 20 BACKGROUND))
(check-expect (ufo-render (make-posn 20 100) BACKGROUND)
              (place-image UFO 20 100 BACKGROUND))
(define (ufo-render u im)
  (place-image
   UFO
   (posn-x u) (posn-y u)
   im))

; Missile Image -> Image
; adds m to the given image im
(check-expect (missile-render (make-posn 22 103) BACKGROUND)
              (place-image MISSILE 22 103 BACKGROUND))
(check-expect (missile-render (make-posn 32 (- HEIGHT TANK-HEIGHT 10))
                              BACKGROUND)
              (place-image MISSILE 32 (- HEIGHT TANK-HEIGHT 10) BACKGROUND))
(define (missile-render m im)
  (place-image
   MISSILE
   (posn-x m) (posn-y m)
   im))
