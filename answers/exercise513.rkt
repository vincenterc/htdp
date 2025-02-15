#lang htdp/isl+

; A Lam is one of:
; – a Symbol
; – L-expr
; – App
(define-struct l-expr [para body])
; A L-expr is a structure:
;   (make-l-expr Symbol Lam)
(define-struct app [fun arg])
; A App is a structure:
;   (make-app Lam Lam)

(define ex1 (make-l-expr 'x 'x))
(define ex2 (make-l-expr 'x 'y))
(define ex3 (make-l-expr 'y (make-l-expr 'x 'y)))
