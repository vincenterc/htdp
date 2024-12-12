#lang htdp/bsl

(require 2htdp/image)

; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation #false means the missile is in the tank;
; Posn says the missile is at that location

; Any -> Boolean
; is v an element of the MissileOrNot collection
(check-expect (missile-or-not? #false) #true)
(check-expect (missile-or-not? (make-posn 9 2)) #true)
(check-expect (missile-or-not? "yellow") #false)
(check-expect (missile-or-not? #true) #false)
(check-expect (missile-or-not? 10) #false)
(check-expect (missile-or-not? empty-image) #false)
(define (missile-or-not? v)
  (or (false? v) (posn? v)))
