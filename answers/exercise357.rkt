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

; Symbol -> String
(define (not-numeric sy)
  (string-append (symbol->string sy)
                 ": "
                 "not numeric!"))

; Symbol -> String
(define (not-func-supported sy)
  (string-append (symbol->string sy)
                 ": "
                 "not a function supported!"))

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; determines the value of ex given f x b
(check-expect
 (eval-definition1
  (make-fun 'k (make-add 1 1))
  'k 'x
  (make-add 'x 3))
 5)
(check-expect
 (eval-definition1
  (make-mul 5 (make-fun 'k (make-add 1 1)))
  'k 'x
  (make-add 'x 3))
 25)
(check-error
 (eval-definition1
  (make-fun 'k (make-add 'y 1))
  'k 'x
  (make-add 'x 3))
 (not-numeric 'y))
(check-error
 (eval-definition1
  (make-mul (make-fun 'i 5)
            (make-fun 'k (make-add 1 1)))
  'k 'x
  (make-add 'x 3))
 (not-func-supported 'i))
(define (eval-definition1 ex f x b)
  (cond [(number? ex) ex]
        [(symbol? ex) (error (not-numeric ex))]
        [(add? ex) (+ (eval-definition1 (add-left ex) f x b)
                      (eval-definition1 (add-right ex) f x b))]
        [(mul? ex) (* (eval-definition1 (mul-left ex) f x b)
                      (eval-definition1 (mul-right ex) f x b))]
        [(fun? ex)
         (if (symbol=? (fun-name ex) f)
             (local ((define value (eval-definition1 (fun-arg ex) f x b))
                     (define plugd (subst b x value)))
               (eval-definition1 plugd f x b))
             (error (not-func-supported (fun-name ex))))]))

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; produces a BSL-fun-expr like ex with all occurrences of x
; replaced by v
(check-expect (subst (make-fun 'k (make-add 'y 1)) 'y 1)
              (make-fun 'k (make-add 1 1)))
(define (subst ex x v)
  (cond [(number? ex) ex]
        [(symbol? ex) (if (symbol=? ex x) v ex)]
        [(add? ex) (make-add (subst (add-left ex) x v)
                             (subst (add-right ex) x v))]
        [(mul? ex) (make-mul (subst (mul-left ex) x v)
                             (subst (mul-right ex) x v))]
        [(fun? ex) (make-fun (fun-name ex)
                             (subst (fun-arg ex) x v))]))

; non-termination
; (eval-definition1
;  (make-fun 'k (make-add 1 1))
;  'k 'x
;  (make-add 'x (make-fun 'k 1)))
