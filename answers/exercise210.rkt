#lang htdp/bsl+

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

; Word -> String
; converts w to a string
(check-expect (word->string '()) "")
(check-expect (word->string (list "a")) "a")
(check-expect (word->string (list "c" "a" "t")) "cat")
(define (word->string w)
  (implode w))
