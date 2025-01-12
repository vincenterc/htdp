#lang htdp/isl+

(define-struct dir [name size readability content])
; A Dir.v2-2 is a structure:
;   (make-dir String N Boolean LOFD)

; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2-2 LOFD)

; A File.v2 is a String.

(define dir-Text
  (make-dir "Text" 1 #true '("part1" "part2" "par3")))
(define dir-Code
  (make-dir "Code" 1 #false '("hang" "draw")))
(define dir-Docs
  (make-dir "Docs" 1 #true '("read!")))
(define dir-Libs
  (make-dir "Libs" 1 #true (list dir-Code dir-Docs)))
(define dir-TS
  (make-dir "TS" 1 #true (list dir-Text "read!" dir-Libs)))
