#lang htdp/bsl

(define (string-insert str i)
  (string-append (substring str 0 i) "_" (substring str i)))

(string=? (string-insert "helloworld" 5) "hello_world")
(string=? (string-insert "helloworld" 10) "helloworld_")
(string=? (string-insert "" 0) "_")
