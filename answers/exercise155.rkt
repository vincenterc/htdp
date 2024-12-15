#lang htdp/bsl

(define-struct layer [color doll])

; An RD (short for Russian doll) is one of:
; – String
; – (make-layer String RD)

; RD -> String
; produces the color of the innermost doll of rd
(check-expect (inner "red") "red")
(check-expect
 (inner (make-layer "yellow" (make-layer "green" "red")))
 "red")
(define (inner rd)
  (cond [(string? rd) rd]
        [else (inner (layer-doll rd))]))

; Application
; (inner (make-layer "yellow" (make-layer "green" "red")))
