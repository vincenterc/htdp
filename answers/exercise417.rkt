#lang htdp/isl+

; in ISL+
; (expt 1.001 1e-12)
; ==
; #i1.000000000000001
; inexact

; in Racket
; (expt 1.001 1e-12)
; 1.000000000000001
; exact
