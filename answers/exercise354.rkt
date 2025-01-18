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

; BSL-value is a Number

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).
(define al-x (list (list 'x 5)))
(define al-x-y (list (list 'x 5)
                     (list 'y 3)))

(define NOT-NUMERIC "Not numeric!")

; BSL-var-expr -> BSL-value
; determines the value of ex if numeric? yields true for it
; signals an error if not
(check-expect (eval-variable 1) 1)
(check-expect (eval-variable (make-add -1 2)) 1)
(check-expect (eval-variable (make-add (make-mul -2 -3) 33)) 39)
(check-expect
 (eval-variable (make-mul (make-add 1 (make-mul 2 3)) 3.14)) 21.98)
(check-error (eval-variable (make-add 'x 3)) NOT-NUMERIC)
(define (eval-variable ex)
  (if (numeric? ex)
      (eval-expression ex)
      (error NOT-NUMERIC)))

; BSL-var-expr AL -> BSL-value
; determines the value of ex if numeric? yields true for it
; after iteratively applying subst to all associations in da
; signals an error if not
(check-expect (eval-variable* 'x al-x) 5)
(check-expect (eval-variable* (make-add 'x 3) al-x) 8)
(check-expect (eval-variable* (make-mul 1/2 (make-mul 'x 3)) al-x) 7.5)
(check-error (eval-variable* (make-add (make-mul 'x 'x)
                                       (make-mul 'y 'y))
                             al-x)
             NOT-NUMERIC)
(check-expect (eval-variable* (make-add (make-mul 'x 'x)
                                        (make-mul 'y 'y))
                              al-x-y)
              34)
(define (eval-variable* ex da)
  (local (; BSL-var-expr
          (define ex-substed
            (foldl (lambda (a es) (subst es (first a) (second a))) ex da)))
    (if (numeric? ex-substed)
        (eval-expression ex-substed)
        (error NOT-NUMERIC))))

; BSL-var-expr -> Boolean
; determines whether a BSL-var-expr is also a BSL-expr
(define (numeric? ex)
  (cond [(number? ex) #true]
        [(symbol? ex) #false]
        [(add? ex) (and (numeric? (add-left ex))
                        (numeric? (add-right ex)))]
        [(mul? ex) (and (numeric? (mul-left ex))
                        (numeric? (mul-right ex)))]))

; BSL-expr -> BSL-value
; computes the value given the BSL-expr be
(define (eval-expression be)
  (cond [(number? be) be]
        [(add? be) (+ (eval-expression (add-left be))
                      (eval-expression (add-right be)))]
        [(mul? be) (* (eval-expression (mul-left be))
                      (eval-expression (mul-right be)))]))

; BSL-var-expr Symbol Number -> BSL-var-expr
; produces a BSL-var-expr like ex with all occurrences of x
; replaced by v
(define (subst ex x v)
  (cond [(number? ex) ex]
        [(symbol? ex) (if (symbol=? ex x) v ex)]
        [(add? ex) (make-add (subst (add-left ex) x v)
                             (subst (add-right ex) x v))]
        [(mul? ex) (make-mul (subst (mul-left ex) x v)
                             (subst (mul-right ex) x v))]))
