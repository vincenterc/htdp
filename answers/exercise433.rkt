#lang htdp/isl+

(define POSITIVE-N "n: positive number expected")

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
; termination (bundle s 0) loops unless s is '()
(check-expect (bundle '() 0) '())
(check-expect (bundle '() 2) '())
(check-error (bundle (explode "abcdefgh") 0) POSITIVE-N)
(check-expect (bundle (explode "abcdefgh") 2)
              (list "ab" "cd" "ef" "gh"))
(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(define (bundle s n)
  (cond [(empty? s) '()]
        [(zero? n) (error POSITIVE-N)]
        [(> n 0)
         (cons (implode (take s n)) (bundle (drop s n) n))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
