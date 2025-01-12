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

; Dir.v2 -> N
; counts the files that the given dir contains
(check-expect (how-many (make-dir "Empty" '())) 0)
(check-expect (how-many dir-Code) 2)
(check-expect (how-many dir-Libs) 3)
(check-expect (how-many dir-TS) 7)
(define (how-many dir)
  (local (; LOFD -> N
          ; counts the files that the given lofd contains
          (define (count-lofd lofd)
            (cond [(empty? lofd) 0]
                  [else (+ (if (string? (first lofd))
                               1
                               (how-many (first lofd)))
                           (count-lofd (rest lofd)))])))
    (count-lofd (dir-content dir))))
