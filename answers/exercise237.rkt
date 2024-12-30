#lang htdp/isl

; Number Number -> Boolean
; is the area of a square with side x larger than c
(check-expect (squared>? 5 10) #true)
(define (squared>? x c)
  (> (* x x) c))