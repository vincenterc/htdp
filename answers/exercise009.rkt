#lang htdp/bsl
(require 2htdp/image)

(define in "hello")

(cond [(string? in) (string-length in)]
      [(image? in) (* (image-width in) (image-height in))]
      [(number? in) (abs in)]
      [(boolean? in) (if in 10 20)]
      [else (error "Not a supported data type")])
