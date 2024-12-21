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

; A List-of-Letters is one of
; - '()
; - (cons Letter List-of-Letters)

(define-struct letter-count [letter count])
; A Letter-Count is a structure:
;   (make-letter-count 1String Number)
; interpretation (make-letter-count l c) represents
; the number c of words that start with the letter l
; in the given dictionary

; A List-of-Letter-Counts is one of:
; - '()
; - (cons Letter-Count List-of-Letter-Counts)

; Dictionary -> List-of-Letter-Counts
; counts how many each letter is used as the first one of a word
; in the given dictionary d
(define (count-by-letter d)
  (count-by-letter/letters LETTERS d))

; List-of-Letters Dictionary -> List-of-Letter-Counts
; counts how many each letter in ls is used as the first one of a word
; in the given dictionary d
(define (count-by-letter/letters ls d)
  (cond [(empty? ls) '()]
        [else (cons (make-letter-count
                     (first ls)
                     (starts-with# (first ls) d))
                    (count-by-letter/letters (rest ls) d))]))

; Letter Dictionary -> Number
; counts the number of words in the dictionary d
; that start with the letter l
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
; (count-by-letter AS-LIST)
