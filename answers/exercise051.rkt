#lang htdp/bsl

(require 2htdp/universe)
(require 2htdp/image)

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three
; possible states that a traffic light may assume

(define INITIAL-STATE "red")

; Number -> TrafficLight
; simulates a traffic light for a given duration
(define (main duration)
  (big-bang INITIAL-STATE
    [on-tick traffic-light-next 1 duration]
    [to-draw render]))

; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

; TrafficLight -> Image
(define (render s)
  (circle 20 "solid" s))
