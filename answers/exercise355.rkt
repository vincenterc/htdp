#lang htdp/isl+

(define-struct add [left right])
(define-struct mul [left right])
; A BSL-var-expr is one of:
; – Number
; – Symbol
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; BSL-value is a Number

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).
(define al-x (list (list 'x 5)))
(define al-x-y (list (list 'x 5)
                     (list 'y 3)))

(define NOT-NUMERIC "Not numeric!")

; BSL-var-expr AL -> BSL-value
; determines the value of e by looking up da
; signals an error if any symbols in e is not in da
(check-expect (eval-var-lookup 'x al-x) 5)
(check-expect (eval-var-lookup (make-add 'x 3) al-x) 8)
(check-expect (eval-var-lookup (make-mul 1/2 (make-mul 'x 3)) al-x) 7.5)
(check-error (eval-var-lookup (make-add (make-mul 'x 'x)
                                        (make-mul 'y 'y))
                              al-x)
             NOT-NUMERIC)
(check-expect (eval-var-lookup (make-add (make-mul 'x 'x)
                                         (make-mul 'y 'y))
                               al-x-y)
              34)
(define (eval-var-lookup e da)
  (cond [(number? e) e]
        [(symbol? e)
         (local (; [Maybe Association]
                 (define assoc (assq e da)))
           (if (boolean? assoc)
               (error NOT-NUMERIC)
               (second assoc)))]
        [(add? e) (+ (eval-var-lookup (add-left e) da)
                     (eval-var-lookup (add-right e) da))]
        [(mul? e) (* (eval-var-lookup (mul-left e) da)
                     (eval-var-lookup (mul-right e) da))]))
