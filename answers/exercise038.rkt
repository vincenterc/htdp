#lang htdp/bsl

; String -> String
; produce a string from str with the last character removed
; given: "hello", expect: "hell"
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))

(string=? (string-remove-last "hello") "hell")
