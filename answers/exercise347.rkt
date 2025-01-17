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

; BSL-value is a Number

; BSL-expr -> BSL-value
; computes the value given the BSL-expr be
(check-expect
 (eval-expression 1) 1)
(check-expect
 (eval-expression (make-add -1 2)) 1)
(check-expect
 (eval-expression (make-add (make-mul -2 -3) 33)) 39)
(check-expect
 (eval-expression (make-mul (make-add 1 (make-mul 2 3)) 3.14)) 21.98)
(define (eval-expression be)
  (cond [(number? be) be]
        [(add? be) (+ (eval-expression (add-left be))
                      (eval-expression (add-right be)))]
        [(mul? be) (* (eval-expression (mul-left be))
                      (eval-expression (mul-right be)))]))
