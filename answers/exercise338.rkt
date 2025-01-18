#lang htdp/isl+

(require htdp/dir)

; A File is a structure:
;   (make-file String N Date String)

; A Dir is a structure:
;   (make-dir String Dir* File*)

; A Dir* is [List-of Dir]

; A File* is [List-of File]

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

; Dir -> N
; counts the files that the given dir contains
(check-expect (how-many (make-dir "Empty" '() '())) 0)
(check-expect (how-many dir-Code) 2)
(check-expect (how-many dir-Libs) 3)
(check-expect (how-many dir-TS) 7)
(define (how-many dir)
  (foldl (lambda (d sum) (+ (how-many d) sum))
         (length (dir-files dir))
         (dir-dirs dir)))

; Application
; (define DIR (create-dir "."))
; (how-many DIR)
