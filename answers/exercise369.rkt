#lang htdp/isl+

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; [List-of Attribute] Symbol -> [Maybe String]
; retrieves the string which is associated with sy in loa
; #false if no such string
(check-expect
 (find-attr '() 'initial)
 #false)
(check-expect
 (find-attr '((initial "X")) 'initial)
 "X")
(check-expect
 (find-attr '((initial "X")) 'name)
 #false)
(check-expect
 (find-attr '((initial "X") (name "sam")) 'name)
 "sam")
(check-expect
 (find-attr '((initial "X") (name "sam")) 'from)
 #false)
(define (find-attr loa sy)
  (local ((define assq-result (assq sy loa)))
    (if (false? assq-result)
        #false
        (second assq-result))))
