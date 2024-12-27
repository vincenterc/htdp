#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define RADIUS 5)
(define WORM (circle RADIUS "solid" "red"))
(define SPEED (* RADIUS 2))

(define WIDTH (* RADIUS 2 17))
(define HEIGHT (* RADIUS 2 17))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

; A Direction is one of:
; - LEFT
; - RIGHT
; - Up
; - DOWN

(define-struct worm [pos dir])
; A Worm is a structure:
;   (make-worm Posn Direction)
; interpretation (make-worm (make-posn x y) d)
; describes a worm x pixel from left, y from top,
; moving in the direction d

(define INITIAL-WORM
  (make-worm
   (make-posn (/ WIDTH 2) (/ HEIGHT 2))
   DOWN))
(define WORM-1
  (make-worm
   (make-posn 25 25)
   DOWN))

; PositiveNumber -> Worm
; A computer game: Worm
(define (worm-main rate)
  (big-bang INITIAL-WORM
    [on-draw render]
    [on-tick tock rate]
    [on-key key-handler]))

; Worm -> Image
; places the worm into the BACKGROUND according to w
(define (render w)
  (place-image
   WORM
   (posn-x (worm-pos w)) (posn-y (worm-pos w))
   BACKGROUND))

; Worm -> Worm
; moves the worm according to w
(check-expect (tock (make-worm (make-posn 20 20) LEFT))
              (make-worm (make-posn 10 20) LEFT))
(check-expect (tock (make-worm (make-posn 20 20) RIGHT))
              (make-worm (make-posn 30 20) RIGHT))
(check-expect (tock (make-worm (make-posn 20 20) UP))
              (make-worm (make-posn 20 10) UP))
(check-expect (tock (make-worm (make-posn 20 20) DOWN))
              (make-worm (make-posn 20 30) DOWN))
(define (tock w)
  (cond [(equal? (worm-dir w) LEFT)
         (make-worm (make-posn (- (posn-x (worm-pos w)) SPEED)
                               (posn-y (worm-pos w)))
                    (worm-dir w))]
        [(equal? (worm-dir w) RIGHT)
         (make-worm (make-posn (+ (posn-x (worm-pos w)) SPEED)
                               (posn-y (worm-pos w)))
                    (worm-dir w))]
        [(equal? (worm-dir w) UP)
         (make-worm (make-posn (posn-x (worm-pos w))
                               (- (posn-y (worm-pos w)) SPEED))
                    (worm-dir w))]
        [(equal? (worm-dir w) DOWN)
         (make-worm (make-posn (posn-x (worm-pos w))
                               (+ (posn-y (worm-pos w)) SPEED))
                    (worm-dir w))]
        [else w]))

; Worm KeyEvent -> Worm
; controls the movement of the worm with the four cardinal arrow keys
(check-expect (key-handler WORM-1 "left")
              (make-worm (worm-pos WORM-1)
                         LEFT))
(check-expect (key-handler WORM-1 "right")
              (make-worm (worm-pos WORM-1)
                         RIGHT))
(check-expect (key-handler WORM-1 "up")
              (make-worm (worm-pos WORM-1)
                         UP))
(check-expect (key-handler WORM-1 "down")
              (make-worm (worm-pos WORM-1)
                         DOWN))
(check-expect (key-handler WORM-1 "a")
              WORM-1)
(define (key-handler w key)
  (cond [(string=? key "left")
         (make-worm
          (worm-pos w)
          LEFT)]
        [(string=? key "right")
         (make-worm
          (worm-pos w)
          RIGHT)]
        [(string=? key "up")
         (make-worm
          (worm-pos w)
          UP)]
        [(string=? key "down")
         (make-worm
          (worm-pos w)
          DOWN)]
        [else w]))

; Application
; (worm-main 0.5)
