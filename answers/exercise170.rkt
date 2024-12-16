#lang htdp/bsl

(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999.
; A Four is a Number between 1000 and 9999.

; A List-of-phones is one of:
; - '()
; - (cons Phone List-of-phones)

; List-of-phones -> List-of-phones
; replaces all occurrences of area code 713 in l with 281
(check-expect (replace '())
              '())
(check-expect (replace (cons (make-phone 207 363 2421) '()))
              (cons (make-phone 207 363 2421) '()))
(check-expect (replace (cons (make-phone 713 776 1099) '()))
              (cons (make-phone 281 776 1099) '()))
(define (replace l)
  (cond [(empty? l) '()]
        [else (cons (area-713->281 (first l))
                    (replace (rest l)))]))

; Phone -> Phone
; replaces area code 713 with 281
(define (area-713->281 p)
  (if (= (phone-area p) 713)
      (make-phone 281 (phone-switch p) (phone-four p))
      p))
