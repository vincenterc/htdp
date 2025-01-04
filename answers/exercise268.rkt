#lang htdp/isl+

(define-struct IR [name description cost price])
; An IR is a structure:
;   (make-IR String String Number Number)
; interpretation (make-IR n d c p) specifies the name of an item n
; ,a description d, the acquisition price c,
; and the recommended sales price p

; [List-of IR] -> [List-of IR]
; sorts a list of inventory records by the difference between
; the cost and the price
(check-expect
 (sort-inventory (list (make-IR "doll" "doll" 20 21)
                       (make-IR "bear" "bear" 9 13)
                       (make-IR "cat" "cat" 5 8)))
 (list (make-IR "bear" "bear" 9 13)
       (make-IR "cat" "cat" 5 8)
       (make-IR "doll" "doll" 20 21)))
(define (sort-inventory l)
  (local (; IR IR -> Boolean
          ; determines whether the difference between
		  ; the cost and the price of the first item
		  ; is larger than that of the second item
          (define (diff-price-cost>? ir1 ir2)
            (> (- (IR-price ir1) (IR-cost ir1))
               (- (IR-price ir2) (IR-cost ir2)))))
    (sort l diff-price-cost>?)))
