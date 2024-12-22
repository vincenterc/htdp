#lang htdp/bsl+

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; String -> Word
; converts s to Word
(check-expect (string->word "") '())
(check-expect (string->word "a") (list "a"))
(check-expect (string->word "cat") (list "c" "a" "t"))
(define (string->word s)
  (explode s))

; Word -> String
; converts w to a string
(check-expect (word->string '()) "")
(check-expect (word->string (list "a")) "a")
(check-expect (word->string (list "c" "a" "t")) "cat")
(define (word->string w)
  (implode w))
