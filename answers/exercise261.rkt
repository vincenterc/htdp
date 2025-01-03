#lang htdp/isl

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)
; An Inventory is one of:
; – '()
; – (cons IR Inventory)

(define inventory-1
  (list (make-IR "a" 0.2)
        (make-IR "b" 1.3)
        (make-IR "c" 0.5)))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(check-expect (extract1 inventory-1)
              (extract2 inventory-1))
(define (extract2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define extract-rest (extract2 (rest an-inv))))
       (cond
         [(<= (IR-price (first an-inv)) 1.0)
          (cons (first an-inv) extract-rest)]
         [else extract-rest]))]))
