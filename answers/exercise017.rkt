#lang htdp/bsl
(require 2htdp/image)

(define (image-classify image)
  (cond [(> (image-height image) (image-width image)) "tall"]
        [(> (image-width image) (image-height image)) "wide"]
        [else "square"]))

(string=? (image-classify (rectangle 10 20 "solid" "black")) "tall")
(string=? (image-classify (rectangle 20 10 "solid" "black")) "wide")
(string=? (image-classify (rectangle 10 10 "solid" "black")) "square")
