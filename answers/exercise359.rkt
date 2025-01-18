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

(define-struct fun-def [name param body])
; A BSL-fun-def is a structure:
;   (make-fun-def Symbol Symbol BSL-fun-expr)

(define (f x) (+ 3 x))
(define fun-f
  (make-fun-def 'f 'x (make-add 3 'x)))

(define (g y) (f (* 2 y)))
(define fun-g
  (make-fun-def 'g 'y (make-fun 'f (make-mul 2 'y))))

(define (h v) (+ (f v) (g v)))
(define fun-h
  (make-fun-def 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v))))

; A BSL-fun-def* is [List-of BSL-fun-def]
(define da-fgh
  (list fun-f fun-g fun-h))

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

; BSL-fun-expr BSL-fun-def* -> Number
; determines the value ex given da
(check-expect
 (eval-function* (make-fun 'h (make-add 1 1)) da-fgh)
 12)
(check-expect
 (eval-function* (make-mul 5 (make-fun 'f (make-add 1 1))) da-fgh)
 25)
(check-expect
 (eval-function*  (make-mul (make-fun 'f 5)
                            (make-fun 'g (make-add 1 1)))
                  da-fgh)
 56)
(check-error
 (eval-function* (make-fun 'i (make-add 1 1)) da-fgh)
 (not-func-supported 'i))
(check-error
 (eval-function* (make-fun 'f (make-add 'z 1)) da-fgh)
 (not-numeric 'z))
(define (eval-function* ex da)
  (cond [(number? ex) ex]
        [(symbol? ex) (error (not-numeric ex))]
        [(add? ex) (+ (eval-function* (add-left ex) da)
                      (eval-function* (add-right ex) da))]
        [(mul? ex) (* (eval-function* (mul-left ex) da)
                      (eval-function* (mul-right ex) da))]
        [(fun? ex)
         (local ((define value (eval-function* (fun-arg ex) da))
                 (define f-def (lookup-def da (fun-name ex)))
                 (define plugd (subst (fun-def-body f-def)
                                      (fun-def-param f-def)
                                      value)))
           (eval-function* plugd da))]))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(define (lookup-def da f)
  (local (; BSL-fun-def*
          (define lookup-result
            (filter (lambda (def) (symbol=? (fun-def-name def) f))
                    da)))
    (if (empty? lookup-result)
        (error (not-func-supported f))
        (first lookup-result))))

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; produces a BSL-fun-expr like ex with all occurrences of x
; replaced by v
(define (subst ex x v)
  (cond [(number? ex) ex]
        [(symbol? ex) (if (symbol=? ex x) v ex)]
        [(add? ex) (make-add (subst (add-left ex) x v)
                             (subst (add-right ex) x v))]
        [(mul? ex) (make-mul (subst (mul-left ex) x v)
                             (subst (mul-right ex) x v))]
        [(fun? ex) (make-fun (fun-name ex)
                             (subst (fun-arg ex) x v))]))