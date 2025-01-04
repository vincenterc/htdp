#lang htdp/isl+

(define EURO->US-DOLLAR 1.06)

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
  (map (lambda (n) (* n EURO->US-DOLLAR)) l))

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
  (map (lambda (f) (* 5/9 (- f 32))) l))

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
  (map (lambda (p) (list (posn-x p) (posn-y p))) l))
