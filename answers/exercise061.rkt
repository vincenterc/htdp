#lang htdp/bsl

; An S-TrafficLight is one of:
; – RED
; – GREEN
; – YELLOW

(define RED 0)
(define GREEN 1)
(define YELLOW 2)

; S-TrafficLight -> S-TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next-symbolic RED) GREEN)
(check-expect (tl-next-symbolic YELLOW) RED)
(define (tl-next-symbolic cs)
  (cond
    [(equal? cs RED) GREEN]
    [(equal? cs GREEN) YELLOW]
    [(equal? cs YELLOW) RED]))
