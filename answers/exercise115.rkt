#lang htdp/bsl

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three
; possible states that a traffic light may assume

; Any -> Boolean
; is the given value an element of TrafficLight
(define (light? x)
  (cond
    [(string? x) (or (string=? "red" x)
                     (string=? "green" x)
                     (string=? "yellow" x))]
    [else #false]))

(define MESSAGE-1
  "The first parameter is not a traffic light, given some other value")

(define MESSAGE-2
  "The second parameter is not a traffic light, given some other value")

; Any Any -> Boolean
; are the two values elements of TrafficLight and,
; if so, are they equal
(check-expect (light=? "red" "red") #true)
(check-expect (light=? "red" "green") #false)
(check-expect (light=? "green" "green") #true)
(check-expect (light=? "yellow" "yellow") #true)
(define (light=? a-value another-value)
  (cond [(not (light? a-value)) (error MESSAGE-1)]
        [(not (light? another-value)) (error MESSAGE-2)]
        [else (string=? a-value another-value)]))
