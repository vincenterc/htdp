#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define LEFT "left")
(define RIGHT "right")

; A Direction is one of:
; - LEFT
; - RIGHT

(define-struct v-cat [x direction happiness])
; A VCat is a structure:
;   (make-VCat Number Direction Number)
; interpretation (make-v-cat x d h) describes a cat where
; x represents the number of pixels between the left border of the scene
; and its rightmost side,
; d indicates the movement direction, and
; h denotes the happiness level.

(define cat1 (bitmap "./images/cat1.png"))
(define cat2 (bitmap "./images/cat2.png"))
(define CAT-WIDTH (image-width cat1))
(define CAT-HEIGHT (image-height cat1))
(define HALF-CAT-WIDTH (/ CAT-WIDTH 2))
(define CAT-SPEED 3)

(define BACKGROUND-WIDTH (* CAT-WIDTH 4))
(define BACKGROUND-HEIGHT (* CAT-HEIGHT 1.5))
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

(define Y-CAT (- BACKGROUND-HEIGHT (/ CAT-HEIGHT 2)))
(define X-CAT-START CAT-WIDTH)
(define X-CAT-END BACKGROUND-WIDTH)

(define MIN-HAPPINESS 0)
(define MAX-HAPPINESS 100)
(define HAPPINESS-DECREASE-RATE 0.1)

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

; VCat -> Image
; manages a walking cat and its happiness level.
(define (happy-cat vc)
  (big-bang vc
    [on-tick tock]
    [to-draw render]
    [on-key feed-or-pet]
    [stop-when stop?]))

; VCat -> VCat
; moves the cat by CAT-SPEED for every clock tick and
; decreases happiness by DECREASE-RATE.
; When the cat reaches either end of the scene, it will turn around.
; The happiness field of vc never falls below 0.
(check-expect (tock (make-v-cat 100 RIGHT 100)) (make-v-cat 103 RIGHT 99.9))
(check-expect (tock (make-v-cat 150 LEFT 50.1)) (make-v-cat 147 LEFT 50))
(check-expect (tock (make-v-cat X-CAT-START LEFT 40.1)) (make-v-cat X-CAT-START RIGHT 40))
(check-expect (tock (make-v-cat X-CAT-END RIGHT 0)) (make-v-cat X-CAT-END LEFT 0))
(define (tock vc)
  (make-v-cat
   (move vc)
   (new-direction vc)
   (if (< (new-happiness (v-cat-happiness vc)) MIN-HAPPINESS)
       MIN-HAPPINESS
       (new-happiness (v-cat-happiness vc)))))

; VCat -> Number
; move the cat, given vc
(define (move vc)
  (cond [(equal? (v-cat-direction vc) LEFT)
         (if (<= (- (v-cat-x vc) CAT-SPEED) X-CAT-START)
             X-CAT-START
             (- (v-cat-x vc) CAT-SPEED))]
        [(equal? (v-cat-direction vc) RIGHT)
         (if (>= (+ (v-cat-x vc) CAT-SPEED) X-CAT-END)
             X-CAT-END
             (+ (v-cat-x vc) CAT-SPEED))]))

; VCat -> Direction
; determines a new direction, given vc
(define (new-direction vc)
  (cond
    [(>= (v-cat-x vc) X-CAT-END) LEFT]
    [(<= (v-cat-x vc) X-CAT-START) RIGHT]
    [else (v-cat-direction vc)]))

; Number -> Number
; produces a new happiness, given h
(define (new-happiness h)
  (- h HAPPINESS-DECREASE-RATE))

; VCat -> Image
; places the cat into the BACKGROUND scene,
; according to vc
(check-expect (render (make-v-cat 100 "right" 0))
              (place-image
               (happiness-gauge 0)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (cat 100)))
(check-expect (render (make-v-cat 151 "right" 50))
              (place-image
               (happiness-gauge 50)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (cat 151)))
(check-expect (render (make-v-cat 200 "right" 100))
              (place-image
               (happiness-gauge 100)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (cat 200)))
(define (render vc)
  (place-image
   (happiness-gauge (v-cat-happiness vc))
   X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
   (cat (v-cat-x vc))))

; Number -> Image
; displays a cat at x (the number of pixels
; between the left border of the scene and its rightmost side)
(check-expect (cat 51) (place-image cat2 (x-right-to-x-center 51) Y-CAT BACKGROUND))
(check-expect (cat 100) (place-image cat1 (x-right-to-x-center 100) Y-CAT BACKGROUND))
(check-expect (cat 151) (place-image cat2 (x-right-to-x-center 151) Y-CAT BACKGROUND))
(check-expect (cat 200) (place-image cat1 (x-right-to-x-center 200) Y-CAT BACKGROUND))
(define (cat x)
  (cond [(odd? x) (place-image cat2 (x-right-to-x-center x) Y-CAT BACKGROUND)]
        [else (place-image cat1 (x-right-to-x-center x) Y-CAT BACKGROUND)]))

; Number -> Number
; transforms x-right (the x-coordinate of the cat's right side)
; into the x-coordinate of the cat's center
(define (x-right-to-x-center x-right)
  (- x-right HALF-CAT-WIDTH))

; Number -> Image
; displays a happiness gauge according to h
(define (happiness-gauge h)
  (overlay/align/offset
   "left" "top"
   (rectangle (* BAR-WIDTH-PER-HAPPINESS h) BAR-HEIGHT "solid" BAR-COLOR)
   (- FRAME-BORDER-WIDTH) (- FRAME-BORDER-WIDTH)
   FRAME))

; VCat KeyEvent -> VCat
; every time the up arrow is pressed,
; the happiness field of vc jumps by 1/3;
; it never exceeds MAX-HAPPINESS.
(define (feed-or-pet vc key)
  (make-v-cat
   (v-cat-x vc)
   (v-cat-direction vc)
   (cond [(string=? key "up")
          (if (> (new-happiness-2 (v-cat-happiness vc)) MAX-HAPPINESS)
              MAX-HAPPINESS
              (new-happiness-2 (v-cat-happiness vc)))]
         [else (v-cat-happiness vc)])))

; Number -> Number
; produces a new happiness, given h
(define (new-happiness-2 h)
  (+ h (/ h 3)))

; VCat -> Boolean
; stops whenever the cat's happiness falls to 0.
(define (stop? vc)
  (if (<= (v-cat-happiness vc) MIN-HAPPINESS)
      #true
      #false))

; Application
; (happy-cat (make-v-cat X-CAT-START "right" MAX-HAPPINESS))
