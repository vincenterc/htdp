#lang htdp/isl+

(require htdp/dir)
(require 2htdp/abstraction)

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

; A Path is [List-of String].
; interpretation directions into a directory tree

; Dir String -> [List-of Path]
; produces the list of paths that lead to f in d
(check-expect (find-all (make-dir "Empty" '() '()) "part1")
              '())
(check-expect (find-all dir-Code "hang")
              (list (list "Code" "hang")))
(check-expect (find-all dir-Code "part2")
              '())
(check-expect (find-all dir-Libs "read!")
              (list (list "Libs" "Docs" "read!")))
(check-expect (find-all dir-Libs "part3")
              '())
(check-expect (find-all dir-TS "read!")
              (list (list "TS" "read!")
                    (list "TS" "Libs" "Docs" "read!")))
(check-expect (find-all dir-TS "part4")
              '())
(define (find-all d f)
  (filter (lambda (p)
            (string=? (list-ref p (- (length p) 1))
                      f))
          (ls-R d)))

; Dir -> [List-of Path]
; lists the paths to all files contained in the given d
(check-expect (ls-R (make-dir "Empty" '() '()))
              '())
(check-expect (ls-R dir-Code)
              (list (list "Code" "hang")
                    (list "Code" "draw")))
(check-expect (ls-R dir-Libs)
              (list (list "Libs" "Code" "hang")
                    (list "Libs" "Code" "draw")
                    (list "Libs" "Docs" "read!")))
(check-expect (ls-R dir-TS)
              (list (list "TS" "read!")
                    (list "TS" "Text" "part1")
                    (list "TS" "Text" "part2")
                    (list "TS" "Text" "part3")
                    (list "TS" "Libs" "Code" "hang")
                    (list "TS" "Libs" "Code" "draw")
                    (list "TS" "Libs" "Docs" "read!")))
(define (ls-R d)
  (append
   (for/list ([f (dir-files d)])
     (list (dir-name d) (file-name f)))
   (for*/list ([dir (dir-dirs d)] [p (ls-R dir)])
     (cons (dir-name d) p))))
