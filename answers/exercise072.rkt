#lang htdp/bsl

(define-struct phone [area number])
; A Phone is a structure:
;   (make-phone Number String)
; interpretation a phone number including
; the area code and the local number

(define-struct phone# [area switch num])
; A Phone# is a structure:
;   (make-phone# Number Number Number)
; interpretation a phone number:
; the first three digits are the area code;
; the next three digits are the code
;   for the phone switch (exchange) of your neighborhood;
; the last four digits are the phone with respect to the neighborhood
