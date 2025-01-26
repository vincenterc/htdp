#lang htdp/isl+

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; produces a list of phone records given a list of names lon and
; a list of phone numbers lopn
; assume lon and lopn are of equal length
; assume the corresponding list items belong to the same person
(check-expect
 (zip '() '())
 '())
(check-expect
 (zip '("John") '("1234567"))
 (list (make-phone-record "John" "1234567")))
(check-expect
 (zip '("John" "Emily") '("1234567" "2234567"))
 (list (make-phone-record "John" "1234567")
       (make-phone-record "Emily" "2234567")))
(define (zip lon lopn)
  (cond
    [(empty? lon) '()]
    [else (cons (make-phone-record (first lon) (first lopn))
                (zip (rest lon) (rest lopn)))]))
