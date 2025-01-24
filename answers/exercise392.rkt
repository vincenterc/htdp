#lang htdp/isl+

(define-struct branch [left right])
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
; A Direction is one of:
; – 'left
; – 'right
; A list of Directions is also called a path.

(define NOT-BRANCH "not a branch")

; TOS [List-of Direction] -> TOS
; produces a TOS given tos and lod
; A Direction tells the function whether to choose the left or
; the right branch in a nonsymbolic tree
(check-expect
 (tree-pick (make-branch (make-branch 'a 'b) 'c)
            '(left right))
 'b)
(check-expect
 (tree-pick (make-branch (make-branch 'a (make-branch 'b 'd)) 'c)
            '(left right))
 (make-branch 'b 'd))
(check-expect
 (tree-pick 'a '()) 'a)
(check-expect
 (tree-pick (make-branch 'a 'b) '()) (make-branch 'a 'b))
(check-error
 (tree-pick 'a '(left right))
 (string-append "a: " NOT-BRANCH))
; Simplified:
(define (tree-pick tos lod)
  (cond [(empty? lod) tos]
        [(symbol? tos)
         (error tos NOT-BRANCH)]
        [(branch? tos)
         (cond [(symbol=? (first lod) 'left)
                (tree-pick (branch-left tos) (rest lod))]
               [(symbol=? (first lod) 'right)
                (tree-pick (branch-right tos) (rest lod))])]))
; Original:
; (define (tree-pick tos lod)
;   (cond [(and (empty? lod) (symbol? tos)) tos]
;         [(and (empty? lod) (branch? tos)) tos]
;         [(and (cons? lod) (symbol? tos))
;          (error tos NOT-BRANCH)]
;         [(and (cons? lod) (branch? tos))
;          (cond [(symbol=? (first lod) 'left)
;                 (tree-pick (branch-left tos) (rest lod))]
;                [(symbol=? (first lod) 'right)
;                 (tree-pick (branch-right tos) (rest lod))])]))
