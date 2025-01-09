#lang htdp/isl+

(require 2htdp/abstraction)

; [List-of [List-of String]] -> [List-of Number]
; determines the number of Strings per item
; in a list of list of string
(check-expect (words-on-line '()) '())
(check-expect (words-on-line '(("hello" "world") ("cat"))) '(2 1))
(define (words-on-line lls)
  (for/list ([ls lls])
    (length ls)))
(check-expect (words-on-line-2 '()) '())
(check-expect (words-on-line-2 '(("hello" "world") ("cat"))) '(2 1))
(define (words-on-line-2 lls)
  (match lls
    ['() '()]
    [(cons fst rst) (cons (length fst) (words-on-line-2 rst))]))
