#lang htdp/bsl

; String -> 1String
; extract the last character from non-empty str
; given: "hello", expect: "o"
(define (string-last str)
  (substring str (- (string-length str) 1)))

(string=? (string-last "hello") "o")
