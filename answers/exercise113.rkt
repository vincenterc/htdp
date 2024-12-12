#lang htdp/bsl

(require 2htdp/image)

; A UFO is a Posn.
; interpretation (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interpretation (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A SIGS is one of:
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a
; space invader game

; Any -> Boolean
; is s an element of the SIGS collection
(check-expect (sigs? (make-aim (make-posn 20 10)
                               (make-tank 28 -3)))
              #true)
(check-expect (sigs? (make-fired (make-posn 20 10)
                                 (make-tank 28 -3)
                                 (make-posn 20 100)))
              #true)
(check-expect (sigs? 10) #false)
(check-expect (sigs? #true) #false)
(check-expect (sigs? "yellow") #false)
(check-expect (sigs? empty-image) #false)
(check-expect (sigs? (make-posn 20 10)) #false)
(check-expect (sigs? (make-tank 28 -3)) #false)
(define (sigs? s)
  (or (aim? s) (fired? s)))

; A Coordinate is one of:
; – a NegativeNumber
; interpretation on the y axis, distance from top
; – a PositiveNumber
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

; Any -> Boolean
; is c an element of the Coordinate collection
(check-expect (coordinate? -1) #true)
(check-expect (coordinate? 10) #true)
(check-expect (coordinate? (make-posn 20 10)) #true)
(check-expect (coordinate? 0) #false)
(check-expect (coordinate? #true) #false)
(check-expect (coordinate? "yellow") #false)
(check-expect (coordinate? empty-image) #false)
(define (coordinate? c)
  (or (and (number? c)
           (not (= c 0)))
      (posn? c)))

(define-struct v-cat [x happiness])
; A VCat is a structure:
;   (make-VCat Number Number)
; interpretation (make-v-cat x h) describes a cat at
; x (the number of pixels between the left border of the scene
; and its rightmost side) with a happiness level h

(define RED "red")
(define BLUE "blue")
(define GREEN "green")

; A Color is one of:
; - RED
; - BLUE
; - GREEN

(define-struct v-cham [x color happiness])
; A VCham is a structure:
;   (make-VCham Number Color Number)
; interpretation (make-v-cham x c h) describes a chameleon at
; x (the number of pixels between the left border of the scene
; and its rightmost side) with a color c and a happiness level h

; A VAnimal is either
; – a VCat
; – a VCham

; Any -> Boolean
; is va an element of the VAnimal collection
(check-expect (v-animal? (make-v-cat 100 100)) #true)
(check-expect (v-animal? (make-v-cham 100 GREEN 100)) #true)
(check-expect (v-animal? 10) #false)
(check-expect (v-animal? #true) #false)
(check-expect (v-animal? "yellow") #false)
(check-expect (v-animal? empty-image) #false)
(check-expect (v-animal? (make-posn 20 10)) #false)
(define (v-animal? va)
  (or (v-cat? va) (v-cham? va)))
