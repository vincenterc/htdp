#lang htdp/bsl

(define str "helloworld")
(define ind "0123456789")
(define i 5)

(string-append (substring str 0 i) (substring str (+ i 1)))