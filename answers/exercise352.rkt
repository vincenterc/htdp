#lang htdp/isl+

(define-struct add [left right])
(define-struct mul [left right])
; A BSL-var-expr is one of:
; – Number
; – Symbol
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr Symbol Number -> BSL-var-expr
; produces a BSL-var-expr like ex with all occurrences of x
; replaced by v
(check-expect (subst 'x 'x 5) 5)
(check-expect (subst (make-add 'x 3) 'x 5)
              (make-add 5 3))
(check-expect (subst (make-mul 1/2 (make-mul 'x 3)) 'x 5)
              (make-mul 1/2 (make-mul 5 3)))
(check-expect (subst (make-add (make-mul 'x 'x)
                               (make-mul 'y 'y))
                     'x 5)
              (make-add (make-mul 5 5)
                        (make-mul 'y 'y)))
(define (subst ex x v)
  (cond [(number? ex) ex]
        [(symbol? ex) (if (symbol=? ex x) v ex)]
        [(add? ex) (make-add (subst (add-left ex) x v)
                             (subst (add-right ex) x v))]
        [(mul? ex) (make-mul (subst (mul-left ex) x v)
                             (subst (mul-right ex) x v))]))
