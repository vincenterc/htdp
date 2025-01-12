#lang htdp/isl+

(define-struct file [name size content])
; A File.v3 is a structure:
;   (make-file String N String)

(define-struct dir [name dirs files])
; A Dir.v3 is a structure:
;   (make-dir String Dir* File*)

; A Dir* is one of:
; – '()
; – (cons Dir.v3 Dir*)

; A File* is one of:
; – '()
; – (cons File.v3 File*)

(define file-part1 (make-file "part1" 99 ""))
(define file-part2 (make-file "part2" 52 ""))
(define file-part3 (make-file "part3" 17 ""))
(define file-hang (make-file "hang" 8 ""))
(define file-draw (make-file "draw" 2 ""))
(define file-read!-1 (make-file "read!" 19 ""))
(define file-read!-2 (make-file "read!" 10 ""))

(define dir-Text
  (make-dir "Text" '() (list file-part1 file-part2 file-part3)))
(define dir-Code
  (make-dir "Code" '() (list file-hang file-draw)))
(define dir-Docs
  (make-dir "Docs" '() (list file-read!-1)))
(define dir-Libs
  (make-dir "Libs" (list dir-Code dir-Docs) '()))
(define dir-TS
  (make-dir "TS" (list dir-Text dir-Libs) (list file-read!-2)))
