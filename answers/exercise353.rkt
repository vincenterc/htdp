#lang htdp/isl+

(define-struct add [left right])
(define-struct mul [left right])
; A BSL-var-expr is one of:
; – Number
; – Symbol
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; A BSL-expr is one of:
; – Number
; – (make-add BSL-expr BSL-expr)
; – (make-mul BSL-expr BSL-expr)

; BSL-var-expr -> Boolean
; determines whether a BSL-var-expr is also a BSL-expr
(check-expect (numeric? 'x) #false)
(check-expect (numeric? (make-add 'x 3)) #false)
(check-expect (numeric? (make-mul 1/2 (make-mul 'x 3))) #false)
(check-expect (numeric? (make-add (make-mul 'x 'x)
                                  (make-mul 'y 'y)))
              #false)
(check-expect (numeric? 1) #true)
(check-expect (numeric? (make-add -1 2)) #true)
(check-expect (numeric? (make-add (make-mul -2 -3) 33)) #true)
(check-expect (numeric? (make-mul (make-add 1 (make-mul 2 3)) 3.14))
              #true)
(define (numeric? ex)
  (cond [(number? ex) #true]
        [(symbol? ex) #false]
        [(add? ex) (and (numeric? (add-left ex))
                        (numeric? (add-right ex)))]
        [(mul? ex) (and (numeric? (mul-left ex))
                        (numeric? (mul-right ex)))]))
