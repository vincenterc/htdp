#lang htdp/bsl

; String -> 1String
; extract the first character from str
; given "hello", expect: "h"
(define (string-first str)
  (substring str 0 1))

(string=? (string-first "hello") "h")
