#lang htdp/bsl

(require 2htdp/batch-io)

(define-struct word-count [letters# words# lines#])
; A WordCount is a structure:
;   (make-word-count Number Number Number)
; interpretation (make-word-count l w ln) represents
; l letters, w words and ln lines

; An LN is one of:
; – '()
; – (cons Los LN)
; interpretation a list of lines, each is a list of Strings

; A List-of-strings is one of
; - '()
; - (cons String List-of-strings)

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())

(define ln0 '())
(define ln1 (cons line0 (cons line1 '())))

; String -> WordCount
; counts the number of 1Strings, words and lines in a given file
(define (wc file)
  (count (read-words/line file)))

; LN -> WordCount
; counts the number of 1Strings, words and lines given ln
(check-expect (count ln0) (make-word-count 0 0 0))
(check-expect (count ln1) (make-word-count 10 2 2))
(define (count ln)
  (make-word-count (count-letters ln)
                   (count-words ln)
                   (count-lines ln)))

; LN -> Number
; counts the number of letters given ln
(check-expect (count-letters ln0) 0)
(check-expect (count-letters ln1) 10)
(define (count-letters ln)
  (cond [(empty? ln) 0]
        [else (+ (count-letters-los (first ln))
                 (count-letters (rest ln)))]))

; List-of-strings -> Number
; counts the number of letters given los
(check-expect (count-letters-los line0) 10)
(check-expect (count-letters-los line1) 0)
(define (count-letters-los los)
  (cond [(empty? los) 0]
        [else (+ (length (explode (first los)))
                 (count-letters-los (rest los)))]))

; LN -> Number
; counts the number of words given ln
(check-expect (count-words ln0) 0)
(check-expect (count-words ln1) 2)
(define (count-words ln)
  (cond [(empty? ln) 0]
        [else (+ (length (first ln))
                 (count-words (rest ln)))]))

; LN -> Number
; counts the number of lines given ln
(check-expect (count-lines ln0) 0)
(check-expect (count-lines ln1) 2)
(define (count-lines ln)
  (length ln))

; Application
; (wc "./files/ttt.txt")
