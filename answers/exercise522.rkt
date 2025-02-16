#lang htdp/isl+

(require 2htdp/image)

(define RADIUS 5)
(define DIAMETER (* RADIUS 2))
(define MISS (circle RADIUS "solid" "black"))
(define CANN (circle RADIUS "outline" "black"))
(define BOARD 2)
(define BANK-WIDTH (+ (* DIAMETER 2) (* BOARD 3)))
(define BANK-HEIGHT (+ (* DIAMETER 3) (* BOARD 4)))
(define BANK
  (rectangle BANK-WIDTH BANK-HEIGHT "outline" "black"))
(define BOAT
  (overlay/offset
   (rhombus 6.5 100 "solid" "black")
   0 RADIUS
   (rectangle DIAMETER RADIUS "solid" "black")))
(define RIVER
  (rectangle (* BANK-WIDTH 2) BANK-HEIGHT "outline" "black"))

(define-struct ps [left right boat states])
; A PuzzleState is a structure:
;   (make-ps Bank Bank Boat [List-of PuzzleState])
; accumulator states contains the sequence of states traversed to reach
; the current state
(define-struct bank [miss cann])
; A Bank is a structure:
;   (make-bank N N)
; interpretation (make-bank m c) represents m missionaries
; and c cannibals
; A Boat is one of:
; - 'left
; - 'right

(define initial-puzzle
  (make-ps (make-bank 3 3) (make-bank 0 0) 'left '()))
(define final-puzzle
  (make-ps (make-bank 0 0) (make-bank 3 3) 'right '()))
(define intermediate-puzzle
  (make-ps (make-bank 1 1) (make-bank 2 2) 'left '()))

; PuzzleState -> Boolean
; determines whether all people are on the right river bank
; in the given state ps
(check-expect (final? initial-puzzle) #false)
(check-expect (final? intermediate-puzzle) #false)
(check-expect (final? final-puzzle) #true)
(define (final? ps)
  (local (; Bank
          (define r-bank (ps-right ps)))
    (and (= (bank-miss r-bank) 3)
         (= (bank-cann r-bank) 3))))

; PuzzleState -> Image
; maps the state ps to an image
(define (render-mc ps)
  (beside (render-bank (ps-left ps))
          (render-river (ps-boat ps))
          (render-bank (ps-right ps))))

; Bank -> Image
; produces a bank image given the bank state b
(define (render-bank b)
  (local ((define m (bank-miss b))
          (define c (bank-cann b))
          (define radius+board (+ RADIUS BOARD))
          (define diameter+board (+ DIAMETER BOARD))
          ; N Image Number Image -> Image
          (define (render n img x back)
            (foldl (lambda (i acc)
                     (place-image
                      img
                      x
                      (+ radius+board (* diameter+board i))
                      acc))
                   back
                   (build-list n identity))))
    (render c CANN (+ radius+board diameter+board)
            (render m MISS radius+board BANK))))

; Boat -> Image
; produces a river image given the boat state b
(define (render-river b)
  (local ((define shift (- BANK-WIDTH RADIUS BOARD)))
    (overlay/offset
     BOAT
     (if (symbol=? b 'left) shift (- shift))
     0
     RIVER)))
