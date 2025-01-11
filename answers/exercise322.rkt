#lang htdp/isl+

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define BT-1
  (make-node
   15 'd
   NONE
   (make-node
    24 'i NONE NONE)))
(define BT-2
  (make-node
   15 'd
   (make-node
    87 'h NONE NONE)
   NONE))

; BT Number -> Boolean
; determines whether a given number n occurs in the given BT bt
(check-expect (contains-bt? NONE 15) #false)
(check-expect (contains-bt? BT-1 15) #true)
(check-expect (contains-bt? BT-1 20) #false)
(check-expect (contains-bt? BT-2 87) #true)
(define (contains-bt? bt n)
  (cond [(no-info? bt) #false]
        [else (or (= (node-ssn bt) n)
                  (contains-bt? (node-left bt) n)
                  (contains-bt? (node-right bt) n))]))
