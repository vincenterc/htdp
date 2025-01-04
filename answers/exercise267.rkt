#lang htdp/isl+

(define EURO->US-DOLLAR 1.06)

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; constructs a list by applying f to each item on lx
; (map f (list x-1 ... x-n)) == (list (f x-1) ... (f x-n))
; (define (map f lx) ...)

; A PositiveNumber is a Number greater than/equal to 0

; [List-of PositiveNumber] -> [List-of PositiveNumber]
; converts a list of US$ amounts into a list of € amounts
; based on an exchange rate EURO->US-DOLLAR
(check-expect (convert-euro '())
              '())
(check-expect (convert-euro '(1))
              '(1.06))
(check-expect (convert-euro '(5 1))
              '(5.3 1.06))
(define (convert-euro l)
  (local (; PositiveNumber -> PositiveNumber
          ; converts a US$ amount into a € amount
          (define (euro->us-dollar n)
            (* n EURO->US-DOLLAR)))
    (map euro->us-dollar l)))

; [List-of Number] -> [List-of Number]
; converts a list of Fahrenheit measurements
; to a list of Celsius measurements
(check-expect (convertFC '())
              '())
(check-expect (convertFC '(-40))
              '(-40))
(check-expect (convertFC '(-40 32))
              '(-40 0))
(check-expect (convertFC '(-40 32 212))
              '(-40 0 100))
(define (convertFC l)
  (local (; Number -> Number
          ; converts Fahrenheit temperatures
          ; to Celsius temperatures
          (define (f2c f)
            (* 5/9 (- f 32))))
    (map f2c l)))

; A Pair is a list of two items:
;   (cons Number (cons Number '()))

; [List-of Posn] -> [List-of Pair]
; translates a list of Posns
; into a list of lists of pairs of numbers
(check-expect (translate (list (make-posn 47 54)
                               (make-posn 50 50)))
              (list (list 47 54)
                    (list 50 50)))
(define (translate l)
  (local (; Posn -> Pair)
          ; converts a Posn into a Pair
          (define (posn->pair p)
            (list (posn-x p) (posn-y p))))
    (map posn->pair l)))
