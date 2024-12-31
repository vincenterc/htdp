#lang htdp/isl

(define (f x) x)

; (cons f '()) is a value,
; because f is a value

; f is a value.
; (f f) may have a value,
; but it is not a value.

; (f 10) may have a value,
; but it is not a value,
; so (cons f (cons 10 (cons (f 10) '()))) is not a value.
