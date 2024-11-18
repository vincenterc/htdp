#lang htdp/bsl

(define (string-join str1 str2)
  (string-append str1 "_" str2))

(string=? (string-join "hello" "world") "hello_world")
