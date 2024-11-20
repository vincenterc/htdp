#lang htdp/bsl

; String -> String
; produce a string from str with the first character removed
; given: "hello", expect: "ello"
(define (string-rest str)
  (substring str 1))

(string=? (string-rest "hello") "ello")
