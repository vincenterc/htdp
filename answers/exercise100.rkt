#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

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

; SIGS -> SIGS
; a space invader game
(define (si-main s)
  (big-bang s
    [on-tick si-move 0.1]
    [on-draw si-render]
    [on-key si-control]
    [stop-when si-game-over? si-render-final]))

; SIGS -> Image
; renders the given game state on top of BACKGROUND
; for examples see figure 32
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
                    X-TANK-MIN
                    X-TANK-MAX)
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

; SIGS KeyEvent -> SIGS
(check-expect (si-control (make-aim (make-posn 10 20)
                                    (make-tank 28 -3))
                          "right")
              (make-aim (make-posn 10 20)
                        (make-tank 28 3)))
(check-expect (si-control (make-aim (make-posn 10 20)
                                    (make-tank 28 -3))
                          "left")
              (make-aim (make-posn 10 20)
                        (make-tank 28 -3)))
(check-expect (si-control (make-fired (make-posn 10 20)
                                      (make-tank 28 -3)
                                      (make-posn 32 100))
                          "right")
              (make-fired
               (make-posn 10 20)
               (make-tank 28 3)
               (make-posn 32 100)))
(check-expect (si-control (make-fired (make-posn 10 20)
                                      (make-tank 28 -3)
                                      (make-posn 32 100))
                          "left")
              (make-fired
               (make-posn 10 20)
               (make-tank 28 -3)
               (make-posn 32 100)))
(check-expect (si-control (make-aim (make-posn 10 20)
                                    (make-tank 28 -3))
                          " ")
              (make-fired (make-posn 10 20)
                          (make-tank 28 -3)
                          (make-posn 28 Y-MISSILE-START)))
(check-expect (si-control (make-fired (make-posn 10 20)
                                      (make-tank 28 -3)
                                      (make-posn 32 100))
                          " ")
              (make-fired
               (make-posn 10 20)
               (make-tank 28 -3)
               (make-posn 32 100)))
(check-expect (si-control (make-aim (make-posn 10 20)
                                    (make-tank 28 -3))
                          "q")
              (make-aim (make-posn 10 20)
                        (make-tank 28 -3)))
(check-expect (si-control (make-fired (make-posn 10 20)
                                      (make-tank 28 -3)
                                      (make-posn 32 100))
                          "a")
              (make-fired
               (make-posn 10 20)
               (make-tank 28 -3)
               (make-posn 32 100)))
(define (si-control s ke)
  (cond [(or (string=? ke "left") (string=? ke "right"))
         (cond [(aim? s) (make-aim (aim-ufo s)
                                   (make-tank (tank-loc (aim-tank s))
                                              (update-tank-vel ke)))]
               [(fired? s) (make-fired (fired-ufo s)
                                       (make-tank (tank-loc (fired-tank s))
                                                  (update-tank-vel ke))
                                       (fired-missile s))])]
        [(string=? ke " ")
         (if (aim? s)
             (make-fired (aim-ufo s)
                         (aim-tank s)
                         (make-posn (tank-loc (aim-tank s))
                                    Y-MISSILE-START))
             s)]
        [else s]))

; KeyEvent -> Number
; update the velocity of tank, given ke
(define (update-tank-vel ke)
  (cond [(string=? ke "left") (- TANK-SPEED)]
        [(string=? ke "right") TANK-SPEED]
        [else (error "Not supported key event.")]))

; Application
; (si-main (make-aim (make-posn X-UFO-START Y-UFO-START)
;                    (make-tank X-TANK-START TANK-SPEED)))