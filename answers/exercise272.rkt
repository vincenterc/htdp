#lang htdp/isl+

(require 2htdp/image)

; [X] [List-of X] [List-of X] -> [List-of X]
; concatenates the items of two lists
(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))
(define (append-from-fold l1 l2)
  (foldr cons l2 l1))

; [List-of Number] -> Number
; computes the sum of the numbers on l
(check-expect (sum (list 1 2 3)) 6)
(define (sum l)
  (foldr + 0 l))

; [List-of Number] -> Number
; computes the product of the numbers on l
(check-expect (product (list 2 3 4)) 24)
(define (product l)
  (foldl * 1 l))

; [List-of Image] -> Image
; composes horizontally a list of Images
(check-expect
 (compose-images-horizontally
  (list (ellipse 20 70 "solid" "gray")
        (ellipse 20 50 "solid" "darkgray")
        (ellipse 20 30 "solid" "dimgray")
        (ellipse 20 10 "solid" "black")))
 (beside (ellipse 20 70 "solid" "gray")
         (ellipse 20 50 "solid" "darkgray")
         (ellipse 20 30 "solid" "dimgray")
         (ellipse 20 10 "solid" "black")))
(define (compose-images-horizontally l)
  (foldr beside empty-image l))

; [List-of Image] -> Image
; stacks vertically a list of Images
(check-expect
 (stack-images-vertically
  (list (ellipse 70 20 "solid" "gray")
        (ellipse 50 20 "solid" "darkgray")
        (ellipse 30 20 "solid" "dimgray")
        (ellipse 10 20 "solid" "black")))
 (above (ellipse 70 20 "solid" "gray")
        (ellipse 50 20 "solid" "darkgray")
        (ellipse 30 20 "solid" "dimgray")
        (ellipse 10 20 "solid" "black")))
(define (stack-images-vertically l)
  (foldr above empty-image l))
