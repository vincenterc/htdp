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

(define lc1 (make-letter-count "a" 10))
(define lc2 (make-letter-count "b" 20))
(define lc3 (make-letter-count "c" 5))

(define llc1 (list lc1))
(define llc2 (list lc1 lc2 lc3))
(define llc2-sorted (list lc2 lc1 lc3))

; Dictionary -> Letter-Count
; produces the Letter-Count fot the letter
; that occurs most often as the first one in the given Dictionary
(define (most-frequent d)
  (max-count (count-by-letter d)))

; Dictionary -> Letter-Count
; produces the Letter-Count fot the letter
; that occurs most often as the first one in the given Dictionary
(define (most-frequent.v2 d)
  (first (sort-by-count (count-by-letter d))))

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

; List-of-Letter-Counts -> Letter-Count
; produces the Letter-Count with the maximum count in llc
(check-expect (max-count '()) '())
(check-expect (max-count llc1) lc1)
(check-expect (max-count llc2) lc2)
(define (max-count llc)
  (cond [(empty? llc) '()]
        [(empty? (rest llc)) (first llc)]
        [else (if (> (letter-count-count (first llc))
                     (letter-count-count (max-count (rest llc))))
                  (first llc)
                  (max-count (rest llc)))]))

; List-of-Letter-Counts -> List-of-Letter-Counts
; sorts a list of Letter-Counts by count
(check-expect (sort-by-count '()) '())
(check-expect (sort-by-count llc1) llc1)
(check-expect (sort-by-count llc2) llc2-sorted)
(define (sort-by-count llc)
  (cond [(empty? llc) '()]
        [else (insert-lc (first llc) (sort-by-count (rest llc)))]))

; Letter-Count List-of-Letter-Counts -> List-of-Letter-Counts
; inserts lc into the sorted list of Letter-Counts llc
(check-expect (insert-lc lc1 '()) (list lc1))
(check-expect (insert-lc lc1 (list lc2 lc3)) llc2-sorted)
(define (insert-lc lc llc)
  (cond [(empty? llc) (cons lc '())]
        [else (if (> (letter-count-count lc)
                     (letter-count-count (first llc)))
                  (cons lc llc)
                  (cons (first llc) (insert-lc lc (rest llc))))]))

; Application
; (most-frequent AS-LIST)
; (most-frequent.v2 AS-LIST)
