#lang htdp/bsl
(require 2htdp/image)

(define cat (bitmap "./images/cat1.png"))

(if (>= (image-height cat) (image-width cat))
    "tall"
    "wide")

(define pic (rectangle 20 30 "solid" "blue"))

(if (> (image-height pic) (image-width pic))
    "tall"
    (if (> (image-width pic) (image-height pic))
        "wide"
        "square"))