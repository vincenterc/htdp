#lang htdp/bsl+

(require 2htdp/batch-io)

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; - '()
; - (cons Word List-of-words)

; A List-of-strings is one of:
; – '()
; – (cons String List-of-strings)

; A Dictionary is a List-of-strings.

(define LOCATION "./files/words")
(define DICTIONARY (read-lines LOCATION))

; String -> List-of-strings
; finds all words that the letters of some given word spell
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
   (member? "rat" w) (member? "art" w) (member? "tar" w)))

; String -> Word
; converts s to Word
(check-expect (string->word "") '())
(check-expect (string->word "a") (list "a"))
(check-expect (string->word "cat") (list "c" "a" "t"))
(define (string->word s)
  (explode s))

; Word -> String
; converts w to a string
(check-expect (word->string '()) "")
(check-expect (word->string (list "a")) "a")
(check-expect (word->string (list "c" "a" "t")) "cat")
(define (word->string w)
  (implode w))

; List-of-words -> List-of-strings
; turns all Words in low into Strings
(check-expect (words->strings '()) '())
(check-expect (words->strings (list (list "a")))
              (list "a"))
(check-expect (words->strings (list (list "a") (list "c" "a" "t")))
              (list "a" "cat"))
(define (words->strings low)
  (cond [(empty? low) '()]
        [else (cons (word->string (first low))
                    (words->strings (rest low)))]))

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

; Word -> List-of-words
; creates all rearrangements of the letters in
(check-expect (arrangements '())
              (list '()))
(check-expect (arrangements (list "a"))
              (list (list "a")))
(check-expect (arrangements (list "d" "e"))
              (list (list "d" "e") (list "e" "d")))
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words
           (first w)
           (arrangements (rest w)))]))

; 1String List-of-words -> List-of-words
; produces a list of words with s inserted at the beginning,
; between all letters, and at the end of all words of the given list low
(check-expect (insert-everywhere/in-all-words "e" (list '()))
              (list (list "e")))
(check-expect (insert-everywhere/in-all-words "d" (list (list "e")))
              (list (list "d" "e") (list "e" "d")))
(check-expect
 (insert-everywhere/in-all-words "d" (list (list "e" "f") (list "f" "e")))
 (list (list "d" "e" "f") (list "e" "d" "f") (list "e" "f" "d")
       (list "d" "f" "e") (list "f" "d" "e") (list "f" "e" "d")))
(define (insert-everywhere/in-all-words s low)
  (cond [(empty? low) '()]
        [else (append (prepend-insert-everywhere/word '() s (first low))
                      (insert-everywhere/in-all-words s (rest low)))]))

; Word 1String Word -> List-of-words
; produces a list of words by inserting s at the beginning,
; between all letters, and at the end of post and prepending pre to
; those words
(check-expect (prepend-insert-everywhere/word '() "e" '())
              (list (list "e")))
(check-expect (prepend-insert-everywhere/word '() "d" (list "e"))
              (list (list "d" "e") (list "e" "d")))
(check-expect (prepend-insert-everywhere/word (list "e") "d" '())
              (list (list "e" "d")))
(check-expect
 (prepend-insert-everywhere/word '() "d" (list "e" "f"))
 (list (list "d" "e" "f") (list "e" "d" "f") (list "e" "f" "d")))
(check-expect
 (prepend-insert-everywhere/word (list "e") "d" (list "f"))
 (list (list "e" "d" "f") (list "e" "f" "d")))
(check-expect
 (prepend-insert-everywhere/word (list "e" "f") "d" '())
 (list  (list "e" "f" "d")))
(define (prepend-insert-everywhere/word pre s word)
  (cond [(empty? word) (list (append pre (cons s '())))]
        [else (cons (append pre (cons s word))
                    (prepend-insert-everywhere/word
                     (append pre (list (first word)))
                     s
                     (rest word)))]))
