#lang htdp/bsl

(define-struct vehicle [passengers license fuel])
; A Vehicle is a structure:
;   (make-vehicle Number Number Number)
; interpretation (make-vehicle p l f) describes a vehicle:
; it can carry p passengers, has license plate number l,
; and consumes f miles per gallon of fuel.
; (define (vehicle-consumer v)
;   ... (vehicle-passengers v) ... (vehicle-license v) ... (vehicle-fuel v))
