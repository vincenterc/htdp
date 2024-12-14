#lang htdp/bsl

(require 2htdp/image)

; A List-of-images is one of
; - '()
; - (cons Image List-of-images)

; ImageOrFalse is one of:
; – Image
; – #false

; List-of-images -> ImageOrFalse
; produces the first image on loi that is not a n by n square;
; if no such image is found, produces #false
(check-expect (ill-sized? '() 10) #false)
(check-expect (ill-sized?
               (cons (rectangle 10 10 "solid" "black") '()) 10)
              #false)
(check-expect (ill-sized?
               (cons (rectangle 10 10 "solid" "black") '()) 20)
              (rectangle 10 10 "solid" "black"))
(check-expect (ill-sized?
               (cons (rectangle 20 10 "solid" "black") '()) 10)
              (rectangle 20 10 "solid" "black"))
(check-expect (ill-sized?
               (cons (rectangle 20 10 "solid" "black")
                     (cons (rectangle 10 10 "solid" "black") '())) 10)
              (rectangle 20 10 "solid" "black"))
(check-expect (ill-sized?
               (cons (rectangle 10 10 "solid" "black")
                     (cons (rectangle 10 10 "solid" "black") '())) 10)
              #false)
(check-expect (ill-sized?
               (cons (rectangle 10 10 "solid" "black")
                     (cons (rectangle 10 10 "solid" "black") '())) 30)
              (rectangle 10 10 "solid" "black"))
(define (ill-sized? loi n)
  (cond [(empty? loi) #false]
        [else (if (not (and (= (image-width (first loi)) n)
                            (= (image-height (first loi)) n)))
                  (first loi)
                  (ill-sized? (rest loi) n))]))
