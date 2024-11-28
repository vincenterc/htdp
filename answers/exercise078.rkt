#lang htdp/bsl

; A Letter is one of;
; - 1String "a" through "z" or
; - #false

(define-struct word [l1 l2 l3])
; A world is a structure:
;   (make-word Letter Letter Letter)
; interpretation a word consists of three lowercase letters
