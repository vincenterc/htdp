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

; BT Number -> [Maybe Symbol]
; produces the value of the name filed of the node
; whose ssn filed is n
; produces #false if no such node exists
(check-expect (search-bt NONE 15) #false)
(check-expect (search-bt BT-1 15) 'd)
(check-expect (search-bt BT-1 20) #false)
(check-expect (search-bt BT-2 87) 'h)
(define (search-bt bt n)
  (cond [(no-info? bt) #false]
        [else (if (= (node-ssn bt) n)
                  (node-name bt)
                  (local ((define search-left-bt
                            (search-bt (node-left bt) n)))
                    (if (boolean? search-left-bt)
                        (search-bt (node-right bt) n)
                        search-left-bt)))]))
