#lang htdp/isl+

(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name arg])
; A BSL-fun-expr is one of:
; – Number
; – Symbol
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-func Symbol BSL-fun-expr)

; (k (+ 1 1))
; (make-fun 'k (make-add 1 1))

; (* 5 (k (+ 1 1)))
; (make-mul 5 (make-fun 'k (make-add 1 1)))

; (* (i 5) (k (+ 1 1)))
; (make-mul (make-fun 'i 5)
;           (make-fun 'k (make-add 1 1)))
