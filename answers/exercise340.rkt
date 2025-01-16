#lang htdp/isl+

(require htdp/dir)

; A File is a structure:
;   (make-file String N Date String)

; A Dir is a structure:
;   (make-dir String Dir* File*)

; A Dir* is a [List-of Dir]

; A File* is a [List-of File]

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

; Dir -> [List-of String]
; lists the names of all files and directories in the given dir
(check-expect (ls (make-dir "Empty" '() '()))
              '())
(check-expect (ls dir-Code)
              '("hang" "draw"))
(check-expect (ls dir-Libs)
              '("Code" "hang" "draw" "Docs" "read!"))
(check-expect (ls dir-TS)
              (list "Text" "part1" "part2" "part3" "Libs"
                    "Code" "hang" "draw" "Docs" "read!" "read!"))
(define (ls dir)
  (append (foldr (lambda (d los)
                   (append (list (dir-name d))
                           (ls d)
                           los))
                 '()
                 (dir-dirs dir))
          (map (lambda (f) (file-name f)) (dir-files dir))))
