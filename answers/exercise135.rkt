#lang htdp/bsl

; A List-of-names is one of:
; – '()
; – (cons String List-of-names)
; interpretation a list of invitees, by last name

; List-of-names -> Boolean
; determines whether "Flatt" is on alon
(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))

; Application
; (contains-flatt? (cons "Flatt" (cons "C" '())))
; (contains-flatt?
;   (cons "A" (cons "Flatt" (cons "C" '()))))
