#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define-struct v-cat [x happiness])
; A VCat is a structure:
;   (make-VCat Number Number)
; interpretation (make-v-cat x h) describes a cat at
; x (the number of pixels between the left border of the scene
; and its rightmost side) with a happiness level h.
; The happiness is a number between 0 and 100 (inclusive).

(define RED "red")
(define BLUE "blue")
(define GREEN "green")
; A Color is one of:
; - RED
; - BLUE
; - GREEN

(define-struct v-cham [x color happiness])
; A VCham is a structure:
;   (make-VCham Number Color Number)
; interpretation (make-v-cham x c h) describes a chameleon at
; x (the number of pixels between the left border of the scene
; and its rightmost side) with a color c and a happiness level h
; The happiness is a number between 0 and 100 (inclusive).

; A VAnimal is either
; – a VCat
; – a VCham

(define cat (bitmap "./images/cat1.png"))
(define CAT-WIDTH (image-width cat))
(define HALF-CAT-WIDTH (/ CAT-WIDTH 2))
(define CAT-HEIGHT (image-height cat))

(define cham (bitmap "./images/chameleon.png"))
(define CHAM-WIDTH (image-width cham))
(define HALF-CHAM-WIDTH (/ CHAM-WIDTH 2))
(define CHAM-HEIGHT (image-height cham))

(define WIDTH (* CAT-WIDTH 4))
(define HEIGHT (* CAT-HEIGHT 1.5))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define Y-CAT (- HEIGHT (/ CAT-HEIGHT 2)))

(define Y-CHAM (- HEIGHT (/ CHAM-HEIGHT 2)))

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

; Number VAnimal -> VAnimal
; given loc and a, walks the a across the canvas, starting from loc
(define (cat-cham loc a)
  (big-bang (cond
              [(v-cat? a)
               (make-v-cat loc (v-cat-happiness a))]
              [(v-cham? a)
               (make-v-cham loc (v-cham-color a) (v-cham-happiness a))])
    [on-tick tock]
    [to-draw render]
    [on-key key-handler]))

; VAnimal -> Image
; places an animal into the BACKGROUND scene
(check-expect (render (make-v-cat 50 0))
              (place-image
               (happiness-gauge 0)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (animal (make-v-cat 50 0))))
(check-expect (render (make-v-cat 100 50))
              (place-image
               (happiness-gauge 50)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (animal (make-v-cat 100 50))))
(check-expect (render (make-v-cham 150 GREEN 50))
              (place-image
               (happiness-gauge 50)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (animal (make-v-cham 150 GREEN 50))))
(check-expect (render (make-v-cham 200 GREEN 100))
              (place-image
               (happiness-gauge 100)
               X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
               (animal (make-v-cham 200 GREEN 100))))
(define (render a)
  (place-image
   (happiness-gauge (cond [(v-cat? a) (v-cat-happiness a)]
                          [(v-cham? a) (v-cham-happiness a)]))
   X-HAPPINESS-GAUGE Y-HAPPINESS-GAUGE
   (animal a)))

; Number -> Image
; displays a happiness gauge according to h
(define (happiness-gauge h)
  (overlay/align/offset
   "left" "top"
   (rectangle (* BAR-WIDTH-PER-HAPPINESS h) BAR-HEIGHT "solid" BAR-COLOR)
   (- FRAME-BORDER-WIDTH) (- FRAME-BORDER-WIDTH)
   FRAME))

; VAnimal -> Image
; displays an animal according to a
(check-expect (animal (make-v-cat 100 100))
              (place-image
               cat
               (x-center (make-v-cat 100 100)) Y-CAT
               BACKGROUND))
(check-expect (animal (make-v-cham 100 GREEN 100))
              (place-image
               (overlay cham (cham-back GREEN))
               (x-center (make-v-cham 100 GREEN 100)) Y-CHAM
               BACKGROUND))
(define (animal a)
  (cond [(v-cat? a) (place-image
                     cat
                     (x-center a) Y-CAT
                     BACKGROUND)]
        [(v-cham? a) (place-image
                      (overlay cham (cham-back (v-cham-color a)))
                      (x-center a) Y-CHAM
                      BACKGROUND)]))

; VAnimal -> Number
; generates the x-coordinate of the animal's center
(define (x-center a)
  (cond [(v-cat? a) (- (v-cat-x a) HALF-CAT-WIDTH)]
        [(v-cham? a) (- (v-cham-x a) HALF-CHAM-WIDTH)]))

; Color -> Image
(define (cham-back color)
  (rectangle CHAM-WIDTH
             CHAM-HEIGHT
             "solid"
             color))

; VAnimal -> VAnimal
; moves the animal by 3 pixels for every clock tick and
; decreases happiness by DECREASE-RATE.
; When the x field of a is greater than WIDTH plus the animal's width,
; it resets and starts from 0.
; The happiness field of a never falls below 0.
(check-expect (tock (make-v-cat 20 100)) (make-v-cat 23 99.9))
(check-expect (tock (make-v-cat 372 50.1)) (make-v-cat 375 50))
(check-expect (tock (make-v-cat 375 0)) (make-v-cat 3 0))
(check-expect (tock (make-v-cham 20 GREEN 100)) (make-v-cham 23 GREEN 99.9))
(check-expect (tock (make-v-cham 360 GREEN 50.1)) (make-v-cham 363 GREEN 50))
(check-expect (tock (make-v-cham 363 GREEN 0)) (make-v-cham 3 GREEN 0))
(define (tock a)
  (cond [(v-cat? a)
         (make-v-cat
          (+ (modulo (v-cat-x a) (+ WIDTH CAT-WIDTH)) 3)
          (new-happiness (v-cat-happiness a)))]
        [(v-cham? a)
         (make-v-cham
          (+ (modulo (v-cham-x a) (+ WIDTH CHAM-WIDTH)) 3)
          (v-cham-color a)
          (new-happiness (v-cham-happiness a)))]))

; Number -> Number
; produces a new happiness, given h
(define (new-happiness h)
  (if (< (- h HAPPINESS-DECREASE-RATE) MIN-HAPPINESS)
      MIN-HAPPINESS
      (- h HAPPINESS-DECREASE-RATE)))

; VAnimal KeyEvent -> VAnimal
; Press the up arrow to pet the cat;
; its happiness jumps by 1/3.
; Press the down arrow to feed animals;
; their happiness increases by FEED-POINTS.
; Press "r" to turn the chameleon's color to RED,
; "b" blue and "g" green.
(check-expect (key-handler (make-v-cat 100 60) "up")
              (make-v-cat 100 80))
(check-expect (key-handler (make-v-cat 100 100) "up")
              (make-v-cat 100 100))
(check-expect (key-handler (make-v-cham 100 GREEN 30) "up")
              (make-v-cham 100 GREEN 30))
(check-expect (key-handler (make-v-cat 100 50) "down")
              (make-v-cat 100 52))
(check-expect (key-handler (make-v-cat 100 100) "down")
              (make-v-cat 100 100))
(check-expect (key-handler (make-v-cham 100 GREEN 30) "down")
              (make-v-cham 100 GREEN 32))
(check-expect (key-handler (make-v-cham 100 GREEN 100) "r")
              (make-v-cham 100 RED 100))
(check-expect (key-handler (make-v-cat 100 100) "r")
              (make-v-cat 100 100))
(check-expect (key-handler (make-v-cham 100 RED 100) "b")
              (make-v-cham 100 BLUE 100))
(check-expect (key-handler (make-v-cat 100 100) "b")
              (make-v-cat 100 100))
(check-expect (key-handler (make-v-cham 100 BLUE 100) "g")
              (make-v-cham 100 GREEN 100))
(check-expect (key-handler (make-v-cat 100 100) "g")
              (make-v-cat 100 100))
(define (key-handler a key)
  (cond [(string=? key "up")
         (cond [(v-cat? a)
                (make-v-cat
                 (v-cat-x a)
                 (update-happiness (v-cat-happiness a)
                                   (/ (v-cat-happiness a) 3)))]
               [(v-cham? a) a])]
        [(string=? key "down")
         (cond [(v-cat? a)
                (make-v-cat
                 (v-cat-x a)
                 (update-happiness (v-cat-happiness a) FEED-POINTS))]
               [(v-cham? a)
                (make-v-cham
                 (v-cham-x a)
                 (v-cham-color a)
                 (update-happiness (v-cham-happiness a) FEED-POINTS))])]
        [(string=? key "r") (change-color a RED)]
        [(string=? key "b") (change-color a BLUE)]
        [(string=? key "g") (change-color a GREEN)]
        [else a]))

; VAnimal Color -> VAnimal
(define (change-color a c)
  (cond [(v-cat? a) a]
        [(v-cham? a) (make-v-cham
                      (v-cham-x a)
                      c
                      (v-cham-happiness a))]))

; Number Number -> Number
; given h, returns a happiness level according to dh.
(define (update-happiness h dh)
  (clamp (+ h dh) MIN-HAPPINESS MAX-HAPPINESS))

; Number Number Number -> Number
; returns a value not less than mn and more than mx
(check-expect (clamp 2 1 3) 2)
(check-expect (clamp 5 1 3) 3)
(check-expect (clamp 0 1 3) 1)
(define (clamp val mn mx)
  (max mn (min val mx)))

; Application
; (cat-cham 100 (make-v-cat 0 100))
; (cat-cham 200 (make-v-cham 0 GREEN 100))
