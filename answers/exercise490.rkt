#lang htdp/isl+

; size               1    2      3  n

; recursions of
; elative->absolute  1    2      3  n

; recursions of
; add-to-each        0  1+0  2+1+0  (n - 1) + (n - 2) + ... + 0
;                                   == ((n - 1) * n) / 2
;                                   == n^2 / 2 - n / 2

; total steps
; == n + n^2 / 2 - n / 2
; == n^2 / 2 + n / 2
; == O(n^2)
