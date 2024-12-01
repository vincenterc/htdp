#lang htdp/bsl

(define-struct v-cat [x happiness])
; A VCat is a structure:
;   (make-VCat Number Number)
; interpretation (make-v-cat x h) describes a cat at
; x (the number of pixels between the left border of the scene
; and its rightmost side) with a happiness level h
