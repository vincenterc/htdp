#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A WorldState is a Number.
; interpretation the number is the level of happiness
; between 0 and 100 (inclusive)

(define MIN-HAPPINESS 0)
(define MAX-HAPPINESS 100)
(define DECREASE-RATE 0.1)

(define BAR-WIDTH 200)
(define BAR-HEIGHT 20)
(define BORDER-WIDTH 2)
(define BACKGROUND-COLOR "white")
(define FRAME
  (overlay (rectangle BAR-WIDTH BAR-HEIGHT "solid" "white")
           (rectangle (+ BAR-WIDTH (* BORDER-WIDTH 2))
                      (+ BAR-HEIGHT (* BORDER-WIDTH 2))
                      "solid" "black")))

(define BAR-WIDTH-PER-HAPPINESS (/ BAR-WIDTH MAX-HAPPINESS))

; WorldState -> Image
(define (gauge-prog ws)
  (big-bang ws
    [on-tick tock]
    [to-draw show]
    [on-key key-handler]))

; WorldState -> WorldState
; decreases ws by DECREASE-RATE;
; it never falls below 0.
(check-expect (tock 100) 99.9)
(check-expect (tock 50.1) 50)
(check-expect (tock 0) 0)
(define (tock ws)
  (if (< (new-happiness ws) MIN-HAPPINESS)
      MIN-HAPPINESS
      (new-happiness ws)))

; Number -> Number
; produces a new happiness, given h
(define (new-happiness h)
  (- h DECREASE-RATE))

; WorldState -> Image
; show the level of happiness according ws
(check-expect (show 0) (overlay/align/offset "left" "top" (BAR 0) (- BORDER-WIDTH) (- BORDER-WIDTH) FRAME))
(check-expect (show 50) (overlay/align/offset "left" "top" (BAR 50) (- BORDER-WIDTH) (- BORDER-WIDTH) FRAME))
(check-expect (show 100) (overlay/align/offset "left" "top" (BAR 100) (- BORDER-WIDTH) (- BORDER-WIDTH) FRAME))
(define (show ws)
  (overlay/align/offset "left" "top" (BAR ws) (- BORDER-WIDTH) (- BORDER-WIDTH) FRAME))

(define (BAR ws)
  (rectangle (* BAR-WIDTH-PER-HAPPINESS ws) BAR-HEIGHT "solid" "red"))

; WorldState KeyEvent -> WorldState
; Every time the down arrow key is pressed, ws decreases by 1/5;
; every time the up arrow is pressed, happiness jumps by 1/3.
; ws never exceeds MAX-HAPPINESS.
(define (key-handler ws key)
  (cond [(string=? key "down") (- ws (/ ws 5))]
        [(string=? key "up") (if (> (+ ws (/ ws 3)) MAX-HAPPINESS)
                                 MAX-HAPPINESS
                                 (+ ws (/ ws 3)))]
        [else ws]))

; Application
; (gauge-prog MAX-HAPPINESS)
