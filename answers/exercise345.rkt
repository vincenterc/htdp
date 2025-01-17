#lang htdp/isl+

(define-struct add [left right])
; An Add is a structure:
;   (make-add BSL-expr BSL-expr)
; interpretation (make-add l r) represents (+ l r)

(define-struct mul [left right])
; A Mul is a structure:
;   (make-mul BSL-expr BSL-expr)
; interpretation (make-mul l r) represents (* l r)

; An BSL-expr is one of:
; - Number
; - Add
; - Mul

(+ 10 -10)
(make-add 10 -10)

(+ (* 20 3) 33)
(make-add (make-mul 20 3)
          33)

(+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))
(make-add (make-mul 3.14
                    (make-mul 2 3))
          (make-mul 3.14
                    (make-mul -1 -9)))

(make-add -1 2)
(+ -1 2)

(make-add (make-mul -2 -3) 33)
(+ (* -2 -3) 33)

(make-mul (make-add 1 (make-mul 2 3)) 3.14)
(* (+ 1 (* 2 3)) 3.14)
