#lang htdp/isl+

(require 2htdp/abstraction)

(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999.
; A Four is a Number between 1000 and 9999.

; [List-of Phone] -> [List-of Phone]
; replaces all occurrences of area code 713 in lop with 281
(check-expect (replace '())
              '())
(check-expect (replace (cons (make-phone 207 363 2421) '()))
              (cons (make-phone 207 363 2421) '()))
(check-expect (replace (cons (make-phone 713 776 1099) '()))
              (cons (make-phone 281 776 1099) '()))
(define (replace lop)
  (for/list ([p lop])
    (match p
      [(phone 713 switch four)
       (make-phone 281 switch four)]
      [phone phone])))
