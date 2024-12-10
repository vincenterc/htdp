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

(define aim1 (make-aim (make-posn 20 10) (make-tank 28 -3)))
(define fired1
  (make-fired (make-posn 20 10)
              (make-tank 28 -3)
              (make-posn 28 (- HEIGHT TANK-HEIGHT))))
(define fired2
  (make-fired (make-posn 20 100)
              (make-tank 100 3)
              (make-posn 22 103)))


(define aim1-image
  (place-image
   TANK
   (tank-loc (aim-tank aim1)) Y-TANK
   (place-image
    UFO
    (posn-x (aim-ufo aim1)) (posn-y (aim-ufo aim1))
    BACKGROUND)))

(define fired1-image
  (place-image
   MISSILE
   (posn-x (fired-missile fired1)) (posn-y (fired-missile fired1))
   (place-image
    TANK
    (tank-loc (fired-tank fired1)) Y-TANK
    (place-image
     UFO
     (posn-x (fired-ufo fired1)) (posn-y (fired-ufo fired1))
     BACKGROUND))))

(define fired2-image
  (place-image
   MISSILE
   (posn-x (fired-missile fired2)) (posn-y (fired-missile fired2))
   (place-image
    TANK
    (tank-loc (fired-tank fired2)) Y-TANK
    (place-image
     UFO
     (posn-x (fired-ufo fired2)) (posn-y (fired-ufo fired2))
     BACKGROUND))))
