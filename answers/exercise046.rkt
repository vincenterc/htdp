#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the right-most of the cat

(define cat1 (bitmap "./images/cat1.png"))
(define cat2 (bitmap "./images/cat2.png"))
(define CAT-WIDTH (image-width cat1))
(define CAT-HEIGHT (image-height cat1))
(define HALF-CAT-WIDTH (/ CAT-WIDTH 2))

(define BACKGROUND-WIDTH (* CAT-WIDTH 4))
(define BACKGROUND-HEIGHT (* CAT-HEIGHT 1.5))
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

(define Y-CAT (- BACKGROUND-HEIGHT (/ CAT-HEIGHT 2)))
(define X-CAT-START CAT-WIDTH)

; WorldState -> WorldState
; moves the cat from left to right.
; Whenever the cat disappears on the right,
; it reappears on the left.
(define (cat-prog ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]))

; WorldState -> WorldState
; moves the cat by 3 pixels for every clock tick.
; When ws is greater than BACKGROUND-WIDTH + CAT1-WIDTH,
; it resets and starts from 0.
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ (modulo ws (+ BACKGROUND-WIDTH CAT-WIDTH)) 3))

; WorldState -> Image
; places the cat into the BACKGROUND scene,
; according to ws
(check-expect (render 51) (place-image cat2 (WS-TO-X-CAT 51) Y-CAT BACKGROUND))
(check-expect (render 100) (place-image cat1 (WS-TO-X-CAT 100) Y-CAT BACKGROUND))
(check-expect (render 151) (place-image cat2 (WS-TO-X-CAT 151) Y-CAT BACKGROUND))
(check-expect (render 200) (place-image cat1 (WS-TO-X-CAT 200) Y-CAT BACKGROUND))
(define (render ws)
  (cond [(odd? ws) (place-image cat2 (WS-TO-X-CAT ws) Y-CAT BACKGROUND)]
        [else (place-image cat1 (WS-TO-X-CAT ws) Y-CAT BACKGROUND)]))

(define (WS-TO-X-CAT ws)
  (- ws HALF-CAT-WIDTH))

; Application
(cat-prog X-CAT-START)
