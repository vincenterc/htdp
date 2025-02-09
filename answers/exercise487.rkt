#lang htdp/isl+

; f(n) = 2^n
; g(n) = 1000 * n

; n           10    11    12    13    14    15
; f(n)      1021  2048  4096  8192 16384 32768
; 3 * f(n)  2048  4096  8192 16384 32768 65536
; g(n)     10000 11000 12000 13000 14000 15000

; c = 3
; bigEnough = 13

; for n >= 13
; 3 * f(n) > g(n)
; so g belongs to O(f)

; If n is between 3 and 12, f(n) is better
