#lang htdp/isl+

(define-struct ir [name description cost price])
; An IR is a structure:
;   (make-ir String String Number Number)
; interpretation (make-ir n d c p) specifies the name of an item n
; ,a description d, the acquisition price c,
; and the recommended sales price p

; [List-of IR] -> [List-of IR]
; sorts a list of inventory records by the difference between
; the cost and the price
(check-expect
 (sort-inventory (list (make-ir "doll" "doll" 20 21)
                       (make-ir "bear" "bear" 9 13)
                       (make-ir "cat" "cat" 5 8)))
 (list (make-ir "bear" "bear" 9 13)
       (make-ir "cat" "cat" 5 8)
       (make-ir "doll" "doll" 20 21)))
(define (sort-inventory l)
  (local (; IR IR -> Boolean
          ; determines whether the difference between
		  ; the cost and the price of the first item
		  ; is larger than that of the second item
          (define (diff-price-cost>? ir1 ir2)
            (> (- (ir-price ir1) (ir-cost ir1))
               (- (ir-price ir2) (ir-cost ir2)))))
    (sort l diff-price-cost>?)))
