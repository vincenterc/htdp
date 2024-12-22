#lang htdp/bsl+

(require 2htdp/batch-io)

; A List-of-strings is one of:
; – '()
; – (cons String List-of-strings)

; A Dictionary is a List-of-strings.

(define LOCATION "./files/words")
(define DICTIONARY (read-lines LOCATION))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(check-expect (in-dictionary '()) '())
(check-expect (in-dictionary (list "cta")) '())
(check-expect (in-dictionary (list "cat")) (list "cat"))
(check-expect (in-dictionary (list "cat" "cta" "tca" "tac" "act" "atc"))
              (list "cat" "act"))
(define (in-dictionary los)
  (cond [(empty? los) '()]
        [else (if (in-dictionary? (first los) DICTIONARY)
                  (cons (first los) (in-dictionary (rest los)))
                  (in-dictionary (rest los)))]))

; String Dictionary -> Boolean
; determines whether the string s occurs in the dictionary d
(check-expect (in-dictionary? "cat" '()) #false)
(check-expect (in-dictionary? "cat" (list "dog")) #false)
(check-expect (in-dictionary? "cat" (list "cat" "dog")) #true)
(define (in-dictionary? s d)
  (cond [(empty? d) #false]
        [else (if (string=? (first d) s)
                  #true
                  (in-dictionary? s (rest d)))]))
