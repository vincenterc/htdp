#lang htdp/bsl
(require 2htdp/image)

(define (image-area image)
  (* (image-width image) (image-height image)))

(= (image-area (rectangle 20 10 "solid" "black")) 200)
