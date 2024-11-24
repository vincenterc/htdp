#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the right-most of the cat

(define cat1 (bitmap "./images/cat1.png"))
(define CAT1-WIDTH (image-width cat1))
(define CAT1-HEIGHT (image-height cat1))
(define HALF-CAT1-WIDTH (/ CAT1-WIDTH 2))

(define BACKGROUND-WIDTH (* CAT1-WIDTH 4))
(define BACKGROUND-HEIGHT (* CAT1-HEIGHT 1.5))
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

(define Y-CAT1 (- BACKGROUND-HEIGHT (/ CAT1-HEIGHT 2)))
(define X-CAT1-START CAT1-WIDTH)

; WorldState -> WorldState
(define (cat-prog ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]))

; WorldState -> WorldState
; move the cat by 3 pixels for every clock tick.
; When ws is greater than BACKGROUND-WIDTH + CAT1-WIDTH,
; it resets and start from 0
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ (modulo ws (+ BACKGROUND-WIDTH CAT1-WIDTH)) 3))

; WorldState -> Image
; places the cat into the BACKGROUND scene,
; according to ws
(check-expect (render 50) (place-image cat1 (WS-TO-X-CAT1 50) Y-CAT1 BACKGROUND))
(check-expect (render 100) (place-image cat1 (WS-TO-X-CAT1 100) Y-CAT1 BACKGROUND))
(check-expect (render 150) (place-image cat1 (WS-TO-X-CAT1 150) Y-CAT1 BACKGROUND))
(check-expect (render 200) (place-image cat1 (WS-TO-X-CAT1 200) Y-CAT1 BACKGROUND))
(define (render ws)
  (place-image cat1 (WS-TO-X-CAT1 ws) Y-CAT1 BACKGROUND))

(define (WS-TO-X-CAT1 ws)
  (- ws HALF-CAT1-WIDTH))

; Application
(cat-prog X-CAT1-START)
