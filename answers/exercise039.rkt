#lang htdp/bsl

(require 2htdp/image)

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
  