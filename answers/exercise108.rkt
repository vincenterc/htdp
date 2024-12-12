#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A PCLight is a non-negative number between 0 and 10 (inclusive).

(define WIDTH 70)
(define HEIGHT 60)
(define BACKGROUND (empty-scene WIDTH HEIGHT "black"))

(define STOP (bitmap "./images/pedestrian_traffic_light_red.png"))
(define WALK (bitmap "./images/pedestrian_traffic_light_green.png"))

(define DEFAULT-PCLIGHT 0)
(define MIN-PCLIGHT 0)

(define FONT-SIZE 48)
(define FONT-COLOR-ODD "orange")
(define FONT-COLOR-EVEN "green")

; PCLight -> PCLight
; simulates a pedestrian crossing light.
(define (pclight pcl)
  (big-bang pcl
    [on-draw render]
    [on-tick countdown 1]
    [on-key key-handler]))

; PCLight -> Image
; When PCLight is 0, the light shows STOP;
; when PCLight is 10, it shows WALK;
; when PCLight is between 9 and 1, it displays the digits with
; odd numbers colored orange and even numbers colored green.
(check-expect (render 0) (light STOP))
(check-expect (render 5)
              (light (text "5" FONT-SIZE FONT-COLOR-ODD)))
(check-expect (render 6)
              (light (text "6" FONT-SIZE FONT-COLOR-EVEN)))
(check-expect (render 10) (light WALK))
(define (render pcl)
  (cond [(= pcl 0) (light STOP)]
        [(= pcl 10) (light WALK)]
        [else (light (text
                      (number->string pcl)
                      FONT-SIZE
                      (if (odd? pcl) FONT-COLOR-ODD FONT-COLOR-EVEN)))]))

; Image -> Image
; places an image l into the BACKGROUND scene.
(check-expect (light STOP) (overlay STOP BACKGROUND))
(check-expect (light WALK) (overlay WALK BACKGROUND))
(check-expect (light (text "0" FONT-SIZE FONT-COLOR-EVEN))
              (overlay
               (text "0" FONT-SIZE FONT-COLOR-EVEN)
               BACKGROUND))
(check-expect (light (text "1" FONT-SIZE FONT-COLOR-ODD))
              (overlay
               (text "1" FONT-SIZE FONT-COLOR-ODD)
               BACKGROUND))
(define (light l)
  (overlay l BACKGROUND))

; PCLight -> PCLight
; decreases pcl by 1, it never falls belows 0.
(check-expect (countdown 10) 9)
(check-expect (countdown 5) 4)
(check-expect (countdown 0) 0)
(define (countdown pcl)
  (if (< (- pcl 1) MIN-PCLIGHT)
      0
      (- pcl 1)))

; PCLight -> PCLight
; Press the space bar to switch PCLight from 0 to 10.
(define (key-handler pcl key)
  (cond [(and (= pcl 0) (string=? key " ")) 10]
        [else pcl]))
