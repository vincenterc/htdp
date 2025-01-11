#lang htdp/isl+

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

; A BST (short for binary search tree) is a BT and:
; - NONE is a BST
; - (make-node ssn0 name0 L R) is a BST if
;   -- L is a BST
;   -- R is a BST
;   -- all ssn fields in L are smaller than ssn0
;   -- all ssn fields in R are larger than ssn0

(define BST-1
  (make-node
   15 'd
   NONE
   (make-node
    24 'i NONE NONE)))
(define BST-2
  (make-node
   15 'd
   (make-node
    10 'h NONE NONE)
   NONE))

; BST Number Symbol-> BST
; produces a BST that is just like b and
; that in place of one NONE subtree contains the node structure
; (make-node n s NONE NONE)
(check-expect (inorder (create-bst NONE 20 'm))
              '(20))
(check-expect (inorder (create-bst BST-1 20 'm))
              '(15 20 24))
(check-expect (inorder (create-bst BST-2 13 's))
              '(10 13 15))
(define (create-bst b n s)
  (cond [(no-info? b) (make-node n s NONE NONE)]
        [else (cond [(< n (node-ssn b))
                     (make-node
                      (node-ssn b)
                      (node-name b)
                      (create-bst (node-left b) n s)
                      (node-right b))]
                    [else
                     (make-node
                      (node-ssn b)
                      (node-name b)
                      (node-left b)
                      (create-bst (node-right b) n s))])]))

; BT -> [List-of Number]
; produces the sequence of all the ssn numbers in the tree bt
; as they show up from left to right when looking at a tree drawing
(define (inorder bt)
  (cond [(no-info? bt) '()]
        [else (append (inorder (node-left bt))
                      (list (node-ssn bt))
                      (inorder (node-right bt)))]))
