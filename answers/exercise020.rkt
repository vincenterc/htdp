#lang htdp/bsl

(define (string-delete str i)
  (if (> (string-length str) 0)
      (string-append (substring str 0 i) (substring str (+ i 1)))
      ""))

(string=? (string-delete "hello_world" 5) "helloworld")
(string=? (string-delete "" 0) "")
