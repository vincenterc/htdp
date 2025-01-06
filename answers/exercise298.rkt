#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

(define ROCKET (bitmap "./images/rocket.png" ))

; An ImageStream is a function:
;   [N -> Image]
; interpretation a stream s denotes a series of images
(define (create-rocket-scene height)
  (place-image ROCKET 50 height (empty-scene 100 60)))

; A WorldState is a N
; interpretation the number of clock ticks passed since launched
(define WS-0 0)

; ImageStream N -> WorldState
(define (my-animate s n)
  (big-bang WS-0
    (on-draw s)
    (on-tick (lambda (ws) (+ ws 1)) (/ 1 30) n)))

; Application
; (my-animate create-rocket-scene 60)
