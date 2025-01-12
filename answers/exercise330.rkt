#lang htdp/isl+

; A Dir.v1 (short for directory) is one of:
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)

; A File.v1 is a String.

(define dir-Text '("part1" "part2" "par3"))
(define dir-Code '("hang" "draw"))
(define dir-Docs '("read!"))
(define dir-Libs (list dir-Code dir-Docs))
(define dir-TS (list dir-Text "read!" dir-Libs))
