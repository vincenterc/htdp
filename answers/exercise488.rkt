#lang htdp/isl+

; f(n) = n * log(n)
; g(n) = n^2

; n     1    5  10   100
; f(n)  0 ~3.5  10   200
; g(n)  1   25 100 10000

; c = 1
; bigEnough = 1

; for n >= 1
; g(n) > f(n)
; so f belongs to O(g)
