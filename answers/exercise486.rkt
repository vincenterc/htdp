#lang htdp/isl+

; n         1 2  3  10   100    1000
; n^2 + n   2 6 12 110 10100 1001000
; n^2       1 4  9 100 10000 1000000
; 2 x n^2   2 8 18 200 20000 2000000

; c = 2
; bigEnough = 1

; for 1 >= n
; n^2 + n <= 2 x n^2
; so n^2 + n belongs to O(n^2)
