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

; Dir.v1 -> N
; counts the files that the given dir contains
(check-expect (how-many '()) 0)
(check-expect (how-many dir-Code) 2)
(check-expect (how-many dir-Libs) 3)
(check-expect (how-many dir-TS) 7)
(define (how-many dir)
  (cond [(empty? dir) 0]
        [else (+ (if (string? (first dir))
                     1
                     (how-many (first dir)))
                 (how-many (rest dir)))]))
