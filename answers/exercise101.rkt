#lang htdp/bsl

(require 2htdp/image)

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game

; A UFO is a Posn.
; interpretation (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A MissileOrNot is one of:
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location

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

; SIGS.v2 -> Image
; renders the given game state on top of BACKGROUND
(check-expect (si-render.v2 (make-sigs (make-posn 10 20)
                                       (make-tank 28 -3)
                                       #false))
              (place-image
               TANK 28 Y-TANK (place-image
                               UFO 10 20 BACKGROUND)))
(check-expect (si-render.v2 (make-sigs
                             (make-posn 10 20)
                             (make-tank 28 -3)
                             (make-posn 32 (- HEIGHT TANK-HEIGHT 10))))
              (place-image
               TANK 28 Y-TANK
               (place-image
                UFO 10 20
                (place-image
                 MISSILE 32 (- HEIGHT TANK-HEIGHT 10)
                 BACKGROUND))))
