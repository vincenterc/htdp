#lang htdp/bsl+

(require 2htdp/batch-io)

(define LOCATION "./files/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter Dictionary -> Number
; counts the number of words that start with the letter l
; in the dictionary d
(check-expect (starts-with# "e" '()) 0)
(check-expect
 (starts-with# "e" (list "bent" "creep" "disguise")) 0)
(check-expect
 (starts-with# "e" (list "bent" "disguise" "enemy")) 1)
(check-expect
 (starts-with# "e" (list "bent" "disguise" "enemy" "escort" "hero")) 2)
(define (starts-with# l d)
  (cond [(empty? d) 0]
        [else
         (+ (starts-with# l (rest d))
            (if (string=? (string-ith (first d) 0) l) 1 0))]))

; Application
; (starts-with# "e" AS-LIST)
; (starts-with# "z" AS-LIST)
