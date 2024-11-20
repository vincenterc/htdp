#lang htdp/bsl
(require 2htdp/image)

; Image -> Number
; count the numbere of pixels in img
; given: (rectangle 20 10 "solid" "black"), expect: 200
; gieen: (square 10 "outline" "black"), expect:100
(define (image-area img)
  (* (image-width img) (image-height img)))

(= (image-area (rectangle 20 10 "solid" "black")) 200)
(= (image-area (square 10 "outline" "black")) 100)
