#lang htdp/bsl

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)
; An Lo1S is one of:
; – '()
; – (cons 1String Lo1S)

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))

; String String -> Editor
; produces an Editor given s1 and s2
(check-expect
 (create-editor (implode all) (implode good))
 (make-editor lla good))
(define (create-editor s1 s2)
  (make-editor (reverse (explode s1))
               (explode s2)))
