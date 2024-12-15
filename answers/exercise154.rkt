#lang htdp/bsl

(define-struct layer [color doll])

; An RD (short for Russian doll) is one of:
; – String
; – (make-layer String RD)

; RD -> String
; produces a string of all colors of rd
(check-expect (colors "red") "red")
(check-expect
 (colors (make-layer "yellow" (make-layer "green" "red")))
 "yellow, green, red")
(define (colors rd)
  (cond [(string? rd) rd]
        [else (string-append
               (layer-color rd)
               ", "
               (colors (layer-doll rd)))]))
