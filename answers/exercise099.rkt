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

(define UFO-SPEED-Y 3)
(define UFO-SPEED-X-MAX 3)
(define X-UFO-MIN 0)
(define X-UFO-MAX WIDTH)

(define TANK (rectangle 50 15 "solid" "blue"))
(define TANK-HEIGHT (image-height TANK))

(define MISSILE-SPEED (* UFO-SPEED-Y 2))

; SIGS -> SIGS
; determines to which position the objects move now for every clock tick.
(define (si-move s)
  (cond [(aim? s) (make-aim (move-ufo (aim-ufo s))
                            (move-tank (aim-tank s)))]
        [(fired? s) (make-fired (move-ufo (fired-ufo s))
                                (move-tank (fired-tank s))
                                (move-missile (fired-missile s)))]))

; Tank -> Tank
; moves the tank
(check-expect (move-tank (make-tank 28 -3)) (make-tank 25 -3))
(check-expect (move-tank (make-tank 100 3)) (make-tank 103 3))
(check-expect (move-tank (make-tank 0 -3)) (make-tank 0 -3))
(check-expect (move-tank (make-tank WIDTH 3)) (make-tank WIDTH 3))
(define (move-tank t)
  (make-tank (clamp (+ (tank-loc t) (tank-vel t))
                    X-UFO-MIN
                    X-UFO-MAX)
             (tank-vel t)))

; MISSILE -> MISSILE
; moves the missile
(check-expect (move-missile (make-posn 22 103))
              (make-posn 22 (- 103 MISSILE-SPEED)))
(check-expect (move-missile (make-posn 32 (- HEIGHT TANK-HEIGHT 10)))
              (make-posn 32 (- (- HEIGHT TANK-HEIGHT 10) MISSILE-SPEED)))
(define (move-missile m)
  (make-posn (posn-x m)
             (- (posn-y m) MISSILE-SPEED)))

; UFO -> UFO
; moves the UFO
(define (move-ufo u)
  (move-ufo-proper u (random-x-ufo (posn-x u))))

; UFO -> UFO
; moves the UFO with next-x-ufo
(check-expect (move-ufo-proper (make-posn 10 20) 8)
              (make-posn 8 (+ 20 UFO-SPEED-Y)))
(check-expect (move-ufo-proper (make-posn 20 100) 22)
              (make-posn 22 (+ 100 UFO-SPEED-Y)))
(define (move-ufo-proper u next-x-ufo)
  (make-posn next-x-ufo
             (+ (posn-y u) UFO-SPEED-Y)))

; Number -> Number
(check-random (random-x-ufo 100)
              (clamp (+ 100 (random-range (- UFO-SPEED-X-MAX)
                                          UFO-SPEED-X-MAX))
                     X-UFO-MIN
                     X-UFO-MAX))
(define (random-x-ufo x)
  (clamp (+ x (random-range (- UFO-SPEED-X-MAX) UFO-SPEED-X-MAX))
         X-UFO-MIN
         X-UFO-MAX))

; Number Number -> Number
; generates a random integer between a and b (inclusive).
(define (random-range a b)
  (+ a (random (+ (- b a) 1))));

; Number Number Number -> Number
; returns a value not less than mn and more than mx
(check-expect (clamp 2 1 3) 2)
(check-expect (clamp 5 1 3) 3)
(check-expect (clamp 0 1 3) 1)
(define (clamp val mn mx)
  (max mn (min val mx)))