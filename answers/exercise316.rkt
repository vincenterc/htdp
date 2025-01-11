#lang htdp/isl+

; An Atom is one of:
; – Number
; – String
; – Symbol

; Atom -> Boolean
; determines whether the given value a is an Atom
(check-expect (atom? 1) #true)
(check-expect (atom? "hello") #true)
(check-expect (atom? 'a) #true)
(check-expect (atom? (make-posn 1 1)) #false)
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
