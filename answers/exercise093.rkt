#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define-struct v-cham [x happiness])
; A VCham is a structure:
;   (make-VCham Number Number)
; interpretation (make-v-cham x h) describes a chameleon at
; x (the number of pixels between the left border of the scene
; and its rightmost side) with a happiness level h

(define cham (bitmap "./images/chameleon.png"))
(define CHAM-WIDTH (image-width cham))
(define CHAM-HEIGHT (image-height cham))
(define HALF-CHAM-WIDTH (/ CHAM-WIDTH 2))

(define B-WIDTH (* CHAM-WIDTH 2))
(define B-HEIGHT (* CHAM-HEIGHT 1.3))
(define BACKGROUND
  (beside
   (empty-scene B-WIDTH B-HEIGHT "green")
   (empty-scene B-WIDTH B-HEIGHT "white")
   (empty-scene B-WIDTH B-HEIGHT "red")))
(define BACKGROUND-WIDTH (image-width BACKGROUND))
(define BACKGROUND-HEIGHT (image-height BACKGROUND))

(define Y-CHAM (- BACKGROUND-HEIGHT (/ CHAM-HEIGHT 2)))
(define X-CHAM-START CHAM-WIDTH)

(define MIN-HAPPINESS 0)
(define MAX-HAPPINESS 100)
(define HAPPINESS-DECREASE-RATE 0.1)

(define FEED-POINTS 2)

(define BAR-WIDTH 100)
(define BAR-HEIGHT 10)
(define BAR-WIDTH-PER-HAPPINESS (/ BAR-WIDTH MAX-HAPPINESS))
(define BAR-COLOR "red")

(define FRAME-BORDER-WIDTH 2)
(define FRAME
  (overlay (rectangle BAR-WIDTH BAR-HEIGHT "solid" "white")
           (rectangle (+ BAR-WIDTH (* FRAME-BORDER-WIDTH 2))
                      (+ BAR-HEIGHT (* FRAME-BORDER-WIDTH 2))
                      "solid" "black")))
(define FRAME-WIDTH (image-width FRAME))
(define HALF-FRAME-WIDTH (/ FRAME-WIDTH 2))
(define FRAME-HEIGHT (image-height FRAME))
(define HALF-FRAME-HEIGHT (/ FRAME-HEIGHT 2))

(define X-HAPPINESS-GAUGE
  (+ HALF-FRAME-WIDTH 5))
(define Y-HAPPINESS-GAUGE
  (+ HALF-FRAME-HEIGHT 5))

; VCham -> VCham
; manages a walking chameleon and its happiness level.
(define (happy-cham vc)
  (big-bang vc
    [on-tick tock]
    [to-draw render]
    [on-key key-handler]
    [stop-when stop?]))

; VCham -> VCham
; moves the chameleon by 3 pixels for every clock tick and
; decreases happiness by DECREASE-RATE.
; When the x field of vc is greater than BACKGROUND-WIDTH + CHAM-WIDTH,
; it resets and starts from 0.
; The happiness field of vc never falls below 0.
(check-expect (tock (make-v-cham 20 100)) (make-v-cham 23 99.9))
(check-expect (tock (make-v-cham 312 50.1)) (make-v-cham 315 50))
(check-expect (tock (make-v-cham 315 0)) (make-v-cham 318 0))
(define (tock vc)
  (make-v-cham
   (+ (modulo (v-cham-x vc) (+ BACKGROUND-WIDTH CHAM-WIDTH)) 3)
   (if (< (new-happiness (v-cham-happiness vc)) MIN-HAPPINESS)
       MIN-HAPPINESS
       (new-happiness (v-cham-happiness vc)))))

; Number -> Number
; produces a new happiness, given h
(define (new-happiness h)
  (- h HAPPINESS-DECREASE-RATE))

; VCham -> Image
; places the chameleon into the BACKGROUND scene,
; according to vc
(check-expect (render (make-v-cham 100 0))
              (place-image
               (happiness-gauge 0)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (chameleon 100)))
(check-expect (render (make-v-cham 151 50))
              (place-image
               (happiness-gauge 50)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (chameleon 151)))
(check-expect (render (make-v-cham 200 100))
              (place-image
               (happiness-gauge 100)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (chameleon 200)))
(define (render vc)
  (place-image
   (happiness-gauge (v-cham-happiness vc))
   X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
   (chameleon (v-cham-x vc))))

; Number -> Image
; displays a chameleon at x (the number of pixels
; between the left border of the scene and its rightmost side)
(check-expect (chameleon 51)
              (place-image cham (x-right-to-x-center 51) Y-CHAM BACKGROUND))
(check-expect (chameleon 100)
              (place-image cham (x-right-to-x-center 100) Y-CHAM BACKGROUND))
(check-expect (chameleon 151)
              (place-image cham (x-right-to-x-center 151) Y-CHAM BACKGROUND))
(check-expect (chameleon 200)
              (place-image cham (x-right-to-x-center 200) Y-CHAM BACKGROUND))
(define (chameleon x)
  (place-image cham
               (x-right-to-x-center x) Y-CHAM
               BACKGROUND))

; Number -> Number
; transforms x-right (the x-coordinate of the chameleon's right side)
; into the x-coordinate of the chameleon's center
(define (x-right-to-x-center x-right)
  (- x-right HALF-CHAM-WIDTH))

; Number -> Image
; displays a happiness gauge according to h
(define (happiness-gauge h)
  (overlay/align/offset
   "left" "top"
   (rectangle (* BAR-WIDTH-PER-HAPPINESS h) BAR-HEIGHT "solid" BAR-COLOR)
   (- FRAME-BORDER-WIDTH) (- FRAME-BORDER-WIDTH)
   FRAME))

; VCham KeyEvent -> VCham
; every time the up arrow is pressed,
; the happiness field of vc jumps by 1/3;
; it never exceeds MAX-HAPPINESS.
(check-expect (key-handler (make-v-cham 100 50) "down") (make-v-cham 100 52))
(check-expect (key-handler (make-v-cham 100 50) "up") (make-v-cham 100 50))
(check-expect (key-handler (make-v-cham 100 100) "down") (make-v-cham 100 100))
(define (key-handler vc key)
  (make-v-cham
   (v-cham-x vc)
   (cond [(string=? key "down")
          (if (> (new-happiness-2 (v-cham-happiness vc)) MAX-HAPPINESS)
              MAX-HAPPINESS
              (new-happiness-2 (v-cham-happiness vc)))]
         [else (v-cham-happiness vc)])))

; Number -> Number
; produces a new happiness, given h
(define (new-happiness-2 h)
  (+ h FEED-POINTS))

; VCham -> Boolean
; stops whenever the chameleon's happiness falls to 0.
(define (stop? vc)
  (if (<= (v-cham-happiness vc) MIN-HAPPINESS)
      #true
      #false))

; Application
; (happy-cham (make-v-cham X-CHAM-START MAX-HAPPINESS))
