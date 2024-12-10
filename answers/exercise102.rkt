#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

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
(define HALF-WIDTH (/ WIDTH 2))
(define HEIGHT 200)
(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

(define UFO
  (overlay (circle 8 "solid" "green")
           (rectangle 50 4 "solid" "green")))
(define UFO-HEIGHT (image-height UFO))
(define HALF-UFO-HEIGHT (/ UFO-HEIGHT 2))
(define UFO-SPEED-Y 3)
(define UFO-SPEED-X-MAX 3)
(define X-UFO-MIN 0)
(define X-UFO-MAX WIDTH)
(define X-UFO-START HALF-WIDTH)
(define Y-UFO-START HALF-UFO-HEIGHT)
(define Y-UFO-LANDS (- HEIGHT HALF-UFO-HEIGHT))

(define TANK (rectangle 50 15 "solid" "blue"))
(define TANK-WIDTH (image-width TANK))
(define HALF-TANK-WIDTH (/ TANK-WIDTH 2))
(define TANK-HEIGHT (image-height TANK))
(define HALF-TANK-HEIGHT (/ TANK-HEIGHT 2))
(define TANK-SPEED 3)
(define Y-TANK (- HEIGHT HALF-TANK-HEIGHT))
(define X-TANK-MIN 0)
(define X-TANK-MAX WIDTH)
(define X-TANK-START HALF-TANK-WIDTH)

(define MISSILE (triangle 15 "solid" "black"))
(define MISSILE-HEIGHT (image-height MISSILE))
(define MISSILE-SPEED (* UFO-SPEED-Y 2))
(define Y-MISSILE-START (- HEIGHT TANK-HEIGHT))

; The sum of the radii of the UFO and the circumcircle of the missile.
(define PROXIMITY
  (+ HALF-UFO-HEIGHT (* MISSILE-HEIGHT (/ 2 3))))

(define FONT-SIZE 26)
(define FONT-COLOR "black")
(define WIN-TEXT "You Win")
(define LOSS-TEXT "Game OVer")

; SIGS.v2 -> SIGS.v2
; a space invader game
(define (si-main.v2 s)
  (big-bang s
    [on-tick si-move.v2 0.1]
    [on-draw si-render.v2]
    [on-key si-control.v2]
    [stop-when si-game-over?.v2 si-render-final]))

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
(define (si-render.v2 s)
  (tank-render
   (sigs-tank s)
   (ufo-render (sigs-ufo s)
               (missile-render.v2 (sigs-missile s)
                                  BACKGROUND))))

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

; MissileOrNot Image -> Image
; adds an image of missile m to scene s
(define (missile-render.v2 m s)
  (cond
    [(boolean? m) s]
    [(posn? m)
     (place-image MISSILE (posn-x m) (posn-y m) s)]))

; SIGS.v2 -> SIGS.v2
; determines to which position the objects move now for every clock tick.
(define (si-move.v2 s)
  (make-sigs (move-ufo (sigs-ufo s))
             (move-tank (sigs-tank s))
             (move-missile.v2 (sigs-missile s))))

; Tank -> Tank
; moves the tank
(check-expect (move-tank (make-tank 28 -3)) (make-tank 25 -3))
(check-expect (move-tank (make-tank 100 3)) (make-tank 103 3))
(check-expect (move-tank (make-tank 0 -3)) (make-tank 0 -3))
(check-expect (move-tank (make-tank WIDTH 3)) (make-tank WIDTH 3))
(define (move-tank t)
  (make-tank (clamp (+ (tank-loc t) (tank-vel t))
                    X-TANK-MIN
                    X-TANK-MAX)
             (tank-vel t)))

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

; MissileOrNot -> MissileOrNot
; moves the missile
(check-expect (move-missile.v2 (make-posn 22 103))
              (make-posn 22 (- 103 MISSILE-SPEED)))
(check-expect (move-missile.v2 (make-posn 32 (- HEIGHT TANK-HEIGHT 10)))
              (make-posn 32 (- (- HEIGHT TANK-HEIGHT 10) MISSILE-SPEED)))
(define (move-missile.v2 m)
  (cond
    [(boolean? m) m]
    [(posn? m)
     (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED))]))

; SIGS.v2 KeyEvent -> SIGS.v2
(check-expect (si-control.v2 (make-sigs (make-posn 10 20)
                                        (make-tank 28 -3)
                                        #false)
                             "right")
              (make-sigs (make-posn 10 20)
                         (make-tank 28 3)
                         #false))
(check-expect (si-control.v2 (make-sigs (make-posn 10 20)
                                        (make-tank 28 -3)
                                        #false)
                             "left")
              (make-sigs (make-posn 10 20)
                         (make-tank 28 -3)
                         #false))
(check-expect (si-control.v2 (make-sigs (make-posn 10 20)
                                        (make-tank 28 -3)
                                        (make-posn 32 100))
                             "right")
              (make-sigs
               (make-posn 10 20)
               (make-tank 28 3)
               (make-posn 32 100)))
(check-expect (si-control.v2 (make-sigs (make-posn 10 20)
                                        (make-tank 28 -3)
                                        (make-posn 32 100))
                             "left")
              (make-sigs
               (make-posn 10 20)
               (make-tank 28 -3)
               (make-posn 32 100)))
(check-expect (si-control.v2 (make-sigs (make-posn 10 20)
                                        (make-tank 28 -3)
                                        #false)
                             " ")
              (make-sigs (make-posn 10 20)
                         (make-tank 28 -3)
                         (make-posn 28 Y-MISSILE-START)))
(check-expect (si-control.v2 (make-sigs (make-posn 10 20)
                                        (make-tank 28 -3)
                                        (make-posn 32 100))
                             " ")
              (make-sigs
               (make-posn 10 20)
               (make-tank 28 -3)
               (make-posn 32 100)))
(check-expect (si-control.v2 (make-sigs (make-posn 10 20)
                                        (make-tank 28 -3)
                                        #false)
                             "q")
              (make-sigs (make-posn 10 20)
                         (make-tank 28 -3)
                         #false))
(check-expect (si-control.v2 (make-sigs (make-posn 10 20)
                                        (make-tank 28 -3)
                                        (make-posn 32 100))
                             "a")
              (make-sigs
               (make-posn 10 20)
               (make-tank 28 -3)
               (make-posn 32 100)))
(define (si-control.v2 s ke)
  (cond [(or (string=? ke "left") (string=? ke "right"))
         (make-sigs (sigs-ufo s)
                    (make-tank (tank-loc (sigs-tank s))
                               (update-tank-vel ke))
                    (sigs-missile s))]
        [(string=? ke " ")
         (if (boolean? (sigs-missile s))
             (make-sigs (sigs-ufo s)
                        (sigs-tank s)
                        (make-posn (tank-loc (sigs-tank s))
                                   Y-MISSILE-START))
             s)]
        [else s]))

; KeyEvent -> Number
; update the velocity of tank, given ke
(define (update-tank-vel ke)
  (cond [(string=? ke "left") (- TANK-SPEED)]
        [(string=? ke "right") TANK-SPEED]
        [else (error "Not supported key event.")]))

; SIGS.v2 -> Boolean
; determines whether the game stops.
; The game stops if the UFO lands or if the missile it the UFO.
(check-expect (si-game-over?.v2 (make-sigs
                                 (make-posn 10 20)
                                 (make-tank 28 -3)
                                 #false))
              #false)
(check-expect (si-game-over?.v2 (make-sigs
                                 (make-posn 10 20)
                                 (make-tank 28 -3)
                                 (make-posn 32 (- HEIGHT TANK-HEIGHT 10))))
              #false)
(check-expect (si-game-over?.v2 (make-sigs
                                 (make-posn 20 100)
                                 (make-tank 100 3)
                                 (make-posn 22 103)))
              #true)
(check-expect (si-game-over?.v2 (make-sigs
                                 (make-posn 20 (- HEIGHT HALF-UFO-HEIGHT))
                                 (make-tank 100 3)
                                 (make-posn 22 103)))
              #true)
(define (si-game-over?.v2 s)
  (or (ufo-lands? (sigs-ufo s))
      (missile-hits-ufo? (sigs-ufo s) (sigs-missile s))))

; UFO -> Boolean
; determines if the UFO lands.
(check-expect (ufo-lands? (make-posn 20 (- HEIGHT HALF-UFO-HEIGHT))) #true)
(check-expect (ufo-lands? (make-posn 10 20)) #false)
(define (ufo-lands? u)
  (>= (posn-y u) Y-UFO-LANDS))

; UFO MissileOrNot -> Boolean
; determines if the missiles hits UFO.
(check-expect (missile-hits-ufo?
               (make-posn 10 20)
               #false)
              #false)
(check-expect (missile-hits-ufo?
               (make-posn 10 20)
               (make-posn 32 (- HEIGHT TANK-HEIGHT 10)))
              #false)
(check-expect (missile-hits-ufo?
               (make-posn 20 100)
               (make-posn 22 103))
              #true)
(define (missile-hits-ufo? u m)
  (if (boolean? m)
      #false
      (<= (distance u m) PROXIMITY)))

; SIGS.v2 -> Image
; renders the final state of the game
(check-expect (si-render-final
               (make-sigs (make-posn 20 (- HEIGHT HALF-UFO-HEIGHT))
                          (make-tank 100 3)
                          (make-posn 22 103)))
              (overlay
               (text LOSS-TEXT FONT-SIZE FONT-COLOR)
               (si-render.v2
                (make-sigs (make-posn 20 (- HEIGHT HALF-UFO-HEIGHT))
                           (make-tank 100 3)
                           (make-posn 22 103)))))
(check-expect (si-render-final
               (make-sigs
                (make-posn 20 100)
                (make-tank 100 3)
                (make-posn 22 103)))
              (overlay (text WIN-TEXT FONT-SIZE FONT-COLOR)
                       (si-render.v2 (make-sigs
                                      (make-posn 20 100)
                                      (make-tank 100 3)
                                      (make-posn 22 103)))))
(define (si-render-final s)
  (overlay
   (text (if (ufo-lands? (sigs-ufo s))
             LOSS-TEXT
             WIN-TEXT)
         FONT-SIZE
         FONT-COLOR)
   (si-render.v2 s)))

; Number Number Number -> Number
; returns a value not less than mn and more than mx
(check-expect (clamp 2 1 3) 2)
(check-expect (clamp 5 1 3) 3)
(check-expect (clamp 0 1 3) 1)
(define (clamp val mn mx)
  (max mn (min val mx)))

; Number Number -> Number
; generates a random integer between a and b (inclusive).
(define (random-range a b)
  (+ a (random (+ (- b a) 1))));

; Posn Posn -> Number
; calculates the distance between p1 and p2.
(check-expect (distance (make-posn 3 4) (make-posn 0 0)) 5)
(check-expect (distance (make-posn 1 1) (make-posn 6 13)) 13)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

; Application
(si-main.v2 (make-sigs (make-posn X-UFO-START Y-UFO-START)
                       (make-tank X-TANK-START TANK-SPEED)
                       #false))
