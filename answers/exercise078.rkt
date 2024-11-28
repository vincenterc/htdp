#lang htdp/bsl

; A Letter is one of;
; - 1String "a" through "z" or
; - #false

(define-struct word [letter1 letter2 letter3])
; A Word is a structure:
;   (make-word Letter Letter Letter)
; interpretation a word consists of three Letter(s)
