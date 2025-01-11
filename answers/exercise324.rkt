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

; BT -> [List-of Number]
; produces the sequence of all the ssn numbers in the tree bt
; as they show up from left to right when looking at a tree drawing
(check-expect (inorder BT-1)
              (list 15 24))
(check-expect (inorder BT-2)
              (list 87 15))
(define (inorder bt)
  (cond [(no-info? bt) '()]
        [else (append (inorder (node-left bt))
                      (list (node-ssn bt))
                      (inorder (node-right bt)))]))
