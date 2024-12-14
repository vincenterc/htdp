#lang htdp/bsl

; A List-of-strings is one of:
; – '()
; – (cons String List-of-strings)
; interpretation a list of strings

; String List-of-strings -> Boolean
; determines whether s is on alos
(check-expect (contains? "Flatt" '()) #false)
(check-expect (contains? "Flatt" (cons "Find" '()))
              #false)
(check-expect (contains? "Flatt" (cons "Flatt" '()))
              #true)
(check-expect
 (contains? "Flatt" (cons "X" (cons "Y"  (cons "Z" '()))))
 #false)
(check-expect
 (contains? "Flatt" (cons "A" (cons "Flatt" (cons "C" '()))))
 #true)
(define (contains? s alos)
  (cond
    [(empty? alos) #false]
    [(cons? alos)
     (or (string=? (first alos) s)
         (contains? s (rest alos)))]))

; Application
; (contains? "Flatt" (cons "b" (cons "Flatt" '())))
