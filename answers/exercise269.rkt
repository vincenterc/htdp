#lang htdp/isl+

(define-struct ir [name description cost price])
; An IR is a structure:
;   (make-ir String String Number Number)
; interpretation (make-ir n d c p) specifies the name of an item n
; ,a description d, the acquisition price c,
; and the recommended sales price p

(define list-1
  (list (make-ir "doll" "doll" 20 21)
        (make-ir "bear" "bear" 9 13)
        (make-ir "cat" "cat" 4 7)
        (make-ir "dog" "dog" 5 8)))

; [List-of IR] -> [List-of IR]
; produces a list of inventory records whose sales price is below ua
; given l
(check-expect (eliminate-expensive 15 list-1)
              (list (make-ir "bear" "bear" 9 13)
                    (make-ir "cat" "cat" 4 7)
                    (make-ir "dog" "dog" 5 8)))
(define (eliminate-expensive ua l)
  (local (; [IR] -> Boolean
          ; determines whether the sales price of i
          ; is below ua
          (define (below-ua? i)
            (< (ir-price i) ua)))
    (filter below-ua? l)))

; [List-of IR] -> [List-of IR]
; produces a list of inventory records that do not use the name ty
; given l
(check-expect (recall "bear" list-1)
              (list (make-ir "doll" "doll" 20 21)
                    (make-ir "cat" "cat" 4 7)
                    (make-ir "dog" "dog" 5 8)))
(define (recall ty l)
  (local (; [IR -> Boolean]
          ; determines whether the name of the given inventory item i
          ; is not the same of ty
          (define (not-name-ty? i)
            (not (string=? (ir-name i) ty))))
    (filter not-name-ty? l)))

(define LN-1 (list "a" "b" "c" "d"))
(define LN-2 (list "b" "c" "f" "g"))

; [List-of String] [List-of String] -> [List-of String]
; selects all names from the list l2 that are also on the list l1
(check-expect (selection LN-1 LN-2)
              (list "b" "c"))
(define (selection l1 l2)
  (local (; String -> Boolean
          ; determines whether the name n is on the list l1
          (define (on-list1? n)
            (member? n l1)))
    (filter on-list1? l2)))
