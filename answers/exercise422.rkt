#lang htdp/isl+

; [X] [List-of X] N -> [List-of [List-of X]]
; bundles chunks of lox into a list of list chunks of size n
(check-expect (list->chunks '() 2) '())
(check-expect (list->chunks (explode "abcdefgh") 2)
              '(("a" "b") ("c" "d") ("e" "f") ("g" "h")))
(check-expect (list->chunks (explode "abcdefg") 3)
              '(("a" "b" "c") ("d" "e" "f") ("g")))
(define (list->chunks lox n)
  (cond [(empty? lox) '()]
        [else (cons (take lox n)
                    (list->chunks (drop lox n) n))]))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(check-expect (bundle '() 2) '())
(check-expect (bundle (explode "abcdefgh") 2)
              (list "ab" "cd" "ef" "gh"))
(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(define (bundle s n)
  (map implode (list->chunks s n)))

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
