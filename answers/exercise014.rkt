#lang htdp/bsl

(define (string-last str)
  (if (> (string-length str) 0)
      (string-ith str (- (string-length str) 1))
      ""))

(string=? (string-last "hello") "o")
(string=? (string-last "") "")
