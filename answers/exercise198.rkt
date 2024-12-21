#lang htdp/bsl+

(require 2htdp/batch-io)

(define LOCATION "./files/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A List-of-Dictionarys is one of:
; - '()
; - (cons Dictionary List-of-Dictionarys)

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
(define (most-frequent.v3 d)
  (one-letter-dictionary->letter-count
   (most-words-dictionary (words-by-first-letter d))))

; Dictionary -> Letter-Count
; produces the Letter-Count given d
(define (one-letter-dictionary->letter-count d)
  (make-letter-count
   (string-ith (first d) 0)
   (length d)))

; List-of-Dictionarys -> Dictionary
; determines the dictionary with the most words
; in the list of Dictionarys lod
(define (most-words-dictionary lod)
  (cond [(empty? lod) '()]
        [else (select-most-words-dictionary
               (first lod)
               (most-words-dictionary (rest lod)))]))

; Dictionary Dictionary -> Dictionary
; produces the Dictionary with the most words given d1 and d2
(check-expect (select-most-words-dictionary (list "a1" "a2" "a3")
                                            (list "b1" "b2"))
              (list "a1" "a2" "a3"))
(define (select-most-words-dictionary d1 d2)
  (if (> (length d1) (length d2)) d1 d2))

; Dictionary -> List-of-Dictionarys
; produces a list of Dictionarys, one per Letter, given d
(define (words-by-first-letter d)
  (words-by-first-letter/letters LETTERS d))

; Dictionary -> List-of-Dictionarys
; produces a list of Dictionarys, one per Letter, given d
(define (words-by-first-letter/letters ls d)
  (cond [(empty? ls) '()]
        [else (cons (starts-with (first ls) d)
                    (words-by-first-letter/letters (rest ls) d))]))

; Letter Dictionary -> Dictionary
; produces a Dictionary in which all words start with the letter l
; from d
(check-expect (starts-with "e" '()) '())
(check-expect
 (starts-with "e" (list "bent" "creep" "disguise")) '())
(check-expect
 (starts-with "e" (list "bent" "disguise" "enemy"))
 (list "enemy"))
(check-expect
 (starts-with "e" (list "bent" "disguise" "enemy" "escort" "hero"))
 (list "enemy" "escort"))
(define (starts-with l d)
  (cond [(empty? d) '()]
        [else
         (if (string=? (string-ith (first d) 0) l)
             (cons (first d) (starts-with l (rest d)))
             (starts-with l (rest d)))]))

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
        [else (select-max-count (first llc) (max-count (rest llc)))]))

; Letter-Count Letter-Count -> Letter-Count
; return the Letter-Count with the maximum count
(check-expect (select-max-count (make-letter-count "a" 100)
                                (make-letter-count "b" 150))
              (make-letter-count "b" 150))
(define (select-max-count lc1 lc2)
  (if (> (letter-count-count lc1) (letter-count-count lc2))
      lc1
      lc2))

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
; (check-expect
;   (most-frequent AS-LIST)
;   (most-frequent.v3 AS-LIST))
