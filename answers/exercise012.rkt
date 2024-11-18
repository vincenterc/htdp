#lang htdp/bsl

(define (cvolume length) (* length length length))

(define (csurface length) (* length length 6))

(= (cvolume 2) 8)
(= (csurface 2) 24)
