#lang htdp/isl+

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

(define list-1
  (list (make-IR "doll" 21)
        (make-IR "bear" 13)
        (make-IR "cat" 7)
        (make-IR "dog" 8)))

; [List-of IR] -> [List-of IR]
; produces a list of inventory records
; whose acquisition price is below ua from the list l
(check-expect (eliminate-expensive 15 list-1)
              (list (make-IR "bear" 13)
                    (make-IR "cat" 7)
                    (make-IR "dog" 8)))
(define (eliminate-expensive ua l)
  (filter (lambda (ir) (< (IR-price ir) ua)) l))

; [List-of IR] -> [List-of IR]
; produces a list of inventory records that do not use the name ty
; from the list l
(check-expect (recall "bear" list-1)
              (list (make-IR "doll" 21)
                    (make-IR "cat" 7)
                    (make-IR "dog" 8)))
(define (recall ty l)
  (filter (lambda (ir) (not (string=? (IR-name ir) ty))) l))

(define LN-1 (list "a" "b" "c" "d"))
(define LN-2 (list "b" "c" "f" "g"))

; [List-of String] [List-of String] -> [List-of String]
; selects all names from the list l2 that are also on the list l1
(check-expect (selection LN-1 LN-2)
              (list "b" "c"))
(define (selection l1 l2)
  (filter (lambda (n) (member? n l1)) l2))
