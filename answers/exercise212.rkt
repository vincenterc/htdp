#lang htdp/bsl+

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; - '()
; - (cons Word List-of-words)

(define word1 (list "d" "e"))
(define word2 (list "e" "d"))

(define low1 (list word1 word2))
