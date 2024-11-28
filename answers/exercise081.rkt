#lang htdp/bsl

(define-struct time [hours minutes seconds])
; A Time is a structure:
;   (make-time Number Number Number)
; interpretation a point in time consists of
; hours, minutes and seconds since midnight

; Time -> Number
; calculates the number of seconds that have passed since midnight,
; given t
(check-expect (time->seconds (make-time 12 30 2)) 45002)
(define (time->seconds t)
  (+ (* (time-hours t) 60 60)
     (* (time-minutes t) 60)
     (time-seconds t)))
