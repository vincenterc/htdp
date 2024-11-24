#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; ground of the canvas and the rocket (its height)

(define HEIGHT 300) ; distances in pixels
(define WIDTH  100)
(define YDELTA 3)

(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))

(define CENTER (/ (image-height ROCKET) 2))

; LRCD -> LRCD
(define (main s)
  (big-bang s
    [on-tick fly]
    [to-draw show]
    [on-key launch]
    [stop-when stop?]))

; LRCD -> Image
; renders the state as a resting or flying rocket
(check-expect
 (show "resting")
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))
(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)))
(check-expect
 (show 0)
 (place-image ROCKET 10 (- (- HEIGHT 0) CENTER) BACKG))
(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (- (- HEIGHT HEIGHT) CENTER) BACKG))
(define (show s)
  (cond
    [(string? s)
     (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)]
    [(<= -3 s -1)
     (place-image (text (number->string s) 20 "red")
                  10 (* 3/4 WIDTH)
                  (place-image ROCKET
                               10 (- HEIGHT CENTER)
                               BACKG))]
    [(>= s 0)
     (place-image ROCKET 10 (- (- HEIGHT s) CENTER) BACKG)]))

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed,
; if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) 0)
(check-expect (fly 10) (+ 10 YDELTA))
(check-expect (fly 22) (+ 22 YDELTA))
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) 0 (+ x 1))]
    [(>= x 0) (+ x YDELTA)]))

; LRCD -> Boolean
(check-expect (stop? "resting") #false)
(check-expect (stop? -2) #false)
(check-expect (stop? 53) #false)
(check-expect (stop? HEIGHT) #true)
(define (stop? x)
  (if (and (number? x)
           (= x HEIGHT))
      #true
      #false))
