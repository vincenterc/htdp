#lang htdp/bsl

(define (string-first str)
  (if (> (string-length str) 0)
      (string-ith str 0)
      "" ))

(string=? (string-first "hello") "h")
(string=? (string-first "") "")
