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

; [Maybe Symbol] is one of:
; Symbol
; NONE

; BST Number -> [Maybe Symbol]
; produces the value of the name filed of the node
; whose ssn filed is n
; produces NONE if no such node exists
(check-expect (search-bst BST-1 15) 'd)
(check-expect (search-bst BST-1 24) 'i)
(check-expect (search-bst BST-2 10) 'h)
(check-expect (search-bst BST-2 24) NONE)
(define (search-bst bst n)
  (cond [(no-info? bst) NONE]
        [else (local ((define n-ssn (node-ssn bst)))
                (cond [(= n n-ssn) (node-name bst)]
                      [(< n n-ssn) (search-bst (node-left bst) n)]
                      [else (search-bst (node-right bst) n)]))]))
