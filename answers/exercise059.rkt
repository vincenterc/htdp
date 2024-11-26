#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three
; possible states that a traffic light may assume

(define BULB-RADIUS 10) ; radius of bulbs
(define BORDER 5) ; border width around three bulbs
(define GAP 10) ; width between bulbs

(define BACKGROUND
  (empty-scene 90 30))

(define X-RED
  (+ (* BULB-RADIUS 1) 0 BORDER))
(define X-YELLOW
  (+ (* BULB-RADIUS 3) GAP BORDER))
(define X-GREEN
  (+ (* BULB-RADIUS 5) (* GAP 2) BORDER))
(define Y (+ BULB-RADIUS BORDER))

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))

; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next "red") "green")
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")
(define (tl-next cs)
  (cond
    [(string=? "red" cs) "green"]
    [(string=? "green" cs) "yellow"]
    [(string=? "yellow" cs) "red"]))

; TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render "red") (traffic-light "red"))
(check-expect (tl-render "yellow") (traffic-light "yellow"))
(check-expect (tl-render "green") (traffic-light "green"))
(define (tl-render cs)
  (cond
    [(string=? "red" cs) (traffic-light "red")]
    [(string=? "green" cs) (traffic-light "green")]
    [(string=? "yellow" cs) (traffic-light "yellow")]))

; String String -> Image
; generate a bulb image, given mode and color
(define (bulb mode color)
  (circle BULB-RADIUS mode color))

; String String -> String
; determine the mode based on bg-color and tl-color.
; if bg-color and tl-color are the same, the mode is "solid";
; otherwise, it is "outline"
(check-expect (solid-or-outline "red" "red") "solid")
(check-expect (solid-or-outline "red" "green") "outline")
(define (solid-or-outline bg-color tl-color)
  (if (string=? bg-color tl-color)
      "solid"
      "outline"))

; String -> Image
; generate traffic light image, given color
(define (traffic-light color)
  (place-image
   (bulb (solid-or-outline "red" color) "red")
   X-RED Y
   (place-image
    (bulb (solid-or-outline "yellow" color) "yellow")
    X-YELLOW Y
    (place-image
     (bulb (solid-or-outline "green" color) "green")
     X-GREEN Y
     BACKGROUND))))

; Application
(traffic-light-simulation "red")
