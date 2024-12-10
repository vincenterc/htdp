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
(define UFO-HEIGHT (image-height UFO))
(define HALF-UFO-HEIGHT (/ UFO-HEIGHT 2))
(define Y-UFO-LANDS (- HEIGHT HALF-UFO-HEIGHT))

(define TANK (rectangle 50 15 "solid" "blue"))
(define TANK-HEIGHT (image-height TANK))
(define HALF-TANK-HEIGHT (/ TANK-HEIGHT 2))
(define Y-TANK (- HEIGHT HALF-TANK-HEIGHT))

(define MISSILE (triangle 15 "solid" "black"))
(define MISSILE-HEIGHT (image-height MISSILE))

; The sum of the radii of the UFO and the circumcircle of the missile.
(define PROXIMITY
  (+ HALF-UFO-HEIGHT (* MISSILE-HEIGHT (/ 2 3))))

(define FONT-SIZE 26)
(define FONT-COLOR "black")
(define WIN-TEXT "You Win")
(define LOSS-TEXT "Game OVer")

; SIGS -> Boolean
; determines whether the game stops.
; The game stops if the UFO lands or if the missile it the UFO.
(check-expect (si-game-over? (make-aim
                              (make-posn 10 20)
                              (make-tank 28 -3)))
              #false)
(check-expect (si-game-over? (make-fired
                              (make-posn 10 20)
                              (make-tank 28 -3)
                              (make-posn 32 (- HEIGHT TANK-HEIGHT 10))))
              #false)
(check-expect (si-game-over? (make-fired
                              (make-posn 20 100)
                              (make-tank 100 3)
                              (make-posn 22 103)))
              #true)
(check-expect (si-game-over? (make-fired
                              (make-posn 20 (- HEIGHT HALF-UFO-HEIGHT))
                              (make-tank 100 3)
                              (make-posn 22 103)))
              #true)
(define (si-game-over? s)
  (cond [(aim? s) (ufo-lands? (aim-ufo s))]
        [(fired? s)
         (or
          (ufo-lands? (fired-ufo s))
          (missile-hits-ufo? (fired-ufo s) (fired-missile s)))]))

; UFO -> Boolean
; determines if the UFO lands.
(check-expect (ufo-lands? (make-posn 20 (- HEIGHT HALF-UFO-HEIGHT))) #true)
(check-expect (ufo-lands? (make-posn 10 20)) #false)
(define (ufo-lands? u)
  (>= (posn-y u) Y-UFO-LANDS))

; UFO MISSILE -> Boolean
; determines if the missiles hits UFO.
(check-expect (missile-hits-ufo?
               (make-posn 10 20)
               (make-posn 32 (- HEIGHT TANK-HEIGHT 10)))
              #false)
(check-expect (missile-hits-ufo?
               (make-posn 20 100)
               (make-posn 22 103))
              #true)
(define (missile-hits-ufo? u m)
  (<= (distance u m) PROXIMITY))

; Posn Posn -> Number
; calculates the distance between p1 and p2.
(check-expect (distance (make-posn 3 4) (make-posn 0 0)) 5)
(check-expect (distance (make-posn 1 1) (make-posn 6 13)) 13)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

; SIGS -> Image
; renders the final state of the game
(check-expect (si-render-final
               (make-fired (make-posn 20 (- HEIGHT HALF-UFO-HEIGHT))
                           (make-tank 100 3)
                           (make-posn 22 103)))
              (overlay
               (text LOSS-TEXT FONT-SIZE FONT-COLOR)
               (si-render
                (make-fired (make-posn 20 (- HEIGHT HALF-UFO-HEIGHT))
                            (make-tank 100 3)
                            (make-posn 22 103)))))
(check-expect (si-render-final
               (make-fired
                (make-posn 20 100)
                (make-tank 100 3)
                (make-posn 22 103)))
              (overlay (text WIN-TEXT FONT-SIZE FONT-COLOR)
                       (si-render (make-fired
                                   (make-posn 20 100)
                                   (make-tank 100 3)
                                   (make-posn 22 103)))))
(define (si-render-final s)
  (overlay
   (text (if (ufo-lands? (cond [(aim? s) (aim-ufo s)]
                               [(fired? s) (fired-ufo s)]))
             LOSS-TEXT
             WIN-TEXT)
         FONT-SIZE
         FONT-COLOR)
   (si-render s)))

; SIGS -> Image
; renders the given game state on top of BACKGROUND
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)
     (tank-render
      (fired-tank s)
      (ufo-render (fired-ufo s)
                  (missile-render (fired-missile s)
                                  BACKGROUND)))]))

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
