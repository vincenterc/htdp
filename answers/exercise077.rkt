#lang htdp/bsl

(define-struct time [hour minute second])
; A Time is a structure:
;   (make-time Number Number Number)
; interpretation a point in time consists of
; hours, minutes and seconds since midnight
