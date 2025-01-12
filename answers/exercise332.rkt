#lang htdp/isl+

(define-struct dir [name content])
; A Dir.v2 is a structure:
;   (make-dir String LOFD)

; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)

; A File.v2 is a String.

(define dir-Text
  (make-dir "Text" '("part1" "part2" "par3")))
(define dir-Code
  (make-dir "Code" '("hang" "draw")))
(define dir-Docs
  (make-dir "Docs" '("read!")))
(define dir-Libs
  (make-dir "Libs" (list dir-Code dir-Docs)))
(define dir-TS
  (make-dir "TS" (list dir-Text "read!" dir-Libs)))
