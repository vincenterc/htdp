#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 4))
(define SPACE
  (rectangle (- WHEEL-DISTANCE WHEEL-RADIUS WHEEL-RADIUS) 1 0 "white"))
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))
(define CAR-BODY
  (rectangle (* WHEEL-RADIUS 8) (* WHEEL-RADIUS 2) "solid" "red"))
(define CAR-ROOF
  (rectangle (* WHEEL-RADIUS 4) (* WHEEL-RADIUS 1.5) "solid" "red"))
(define CAR-BODY-ROOF
  (overlay/offset CAR-ROOF
                  0 (+ (/ (image-height CAR-ROOF) 2) (/ (image-height CAR-BODY) 2))
                  CAR-BODY))
(define CAR
  (underlay/offset CAR-BODY-ROOF
                   0 (/ (image-height CAR-BODY-ROOF) 2)
                   BOTH-WHEELS))
(define CAR-WIDTH (image-width CAR))    ; 40
(define HALF-CAR-WIDTH (/ CAR-WIDTH 2))
(define CAR-HEIGHT (image-height CAR))  ; 23

; WorldState -> WorldState
; moves the car by 3 pixels for every clock tick
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ ws 3))

(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define TREE-HEIGHT (image-height TREE))
(define BACKGROUND-WIDTH 200)
(define BACKGROUND-HEIGHT 40)
(define BACKGROUND (place-image TREE
                                (* BACKGROUND-WIDTH 3/4)
                                (- BACKGROUND-HEIGHT (/ TREE-HEIGHT 2))
                                (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT)))
(define Y-CAR (- BACKGROUND-HEIGHT (/ CAR-HEIGHT 2)))
(define X-CAR-START (/ CAR-WIDTH 2))
(define X-CAR-STOP (+ BACKGROUND-WIDTH (/ CAR-WIDTH 2)))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to ws
(check-expect (render 50) (place-image CAR 50 Y-CAR BACKGROUND))
(check-expect (render 100) (place-image CAR 100 Y-CAR BACKGROUND))
(check-expect (render 150) (place-image CAR 150 Y-CAR BACKGROUND))
(check-expect (render 200) (place-image CAR 200 Y-CAR BACKGROUND))
(define (render ws)
  (place-image CAR ws Y-CAR BACKGROUND))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down"
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))

; WorldState -> WorldState
; A WorldState is a Number.
; ws is the number of pixels between
; the left border of the scene and the center of the car
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [on-mouse hyper]
    [to-draw render]))

(main 1)
