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

; A BSL-con-def is a list of two items:
;   (cons Symbol (cons Number '()))

(define-struct fun-def [name param body])
; A BSL-fun-def is a structure:
;   (make-fun-def Symbol Symbol BSL-fun-expr)

; A BSL-def is one of:
; - BSL-con-def
; - BSL-fun-def

; BSL-da-all is [List-of BSL-def]

(define close-to-pi 3.14)
; BSL-con-def
(define con-close-to-pi
  (list 'close-to-pi 3.14))

(define (area-of-circle r)
  (* close-to-pi (* r r)))
; BSL-fun-def
(define fun-area-of-circle
  (make-fun-def
   'area-of-circle
   'r
   (make-mul 'close-to-pi (make-mul 'r 'r))))

(define (volume-of-10-cylinder r)
  (* 10 (area-of-circle r)))
; BSL-fun-def
(define fun-volume-of-10-cylinder
  (make-fun-def
   'volume-of-10-cylinder
   'r
   (make-mul 10 (make-fun 'area-of-circle 'r))))

; BSL-da-all
(define da-all
  (list con-close-to-pi
        fun-area-of-circle
        fun-volume-of-10-cylinder))

; Symbol -> String
(define (con-not-found sy)
  (string-append (symbol->string sy)
                 ": "
                 "no such constant definition can be found!"))
; Symbol -> String
(define (fun-not-found sy)
  (string-append (symbol->string sy)
                 ": "
                 "no such function definition can be found!"))

; BSL-fun-expr BSL-da-all -> Number
; produces the value of ex given da
(check-expect
 (eval-all (make-fun 'area-of-circle 1) da-all)
 3.14)
(check-expect
 (eval-all (make-fun 'volume-of-10-cylinder 1) da-all)
 31.4)
(check-expect
 (eval-all (make-mul 3 'close-to-pi) da-all)
 9.42)
(check-expect
 (eval-all (make-add 3 'close-to-pi) da-all)
 6.14)
(check-error
 (eval-all (make-fun 'f 1) da-all)
 (fun-not-found 'f))
(check-error
 (eval-all (make-add 3 'x) da-all)
 (con-not-found 'x))
(define (eval-all ex da)
  (cond [(number? ex) ex]
        [(symbol? ex) (second (lookup-con-def da ex))]
        [(add? ex) (+ (eval-all (add-left ex) da)
                      (eval-all (add-right ex) da))]
        [(mul? ex) (* (eval-all (mul-left ex) da)
                      (eval-all (mul-right ex) da))]
        [(fun? ex)
         (local ((define value (eval-all (fun-arg ex) da))
                 (define f-def (lookup-fun-def da (fun-name ex)))
                 (define plugd (subst (fun-def-body f-def)
                                      (fun-def-param f-def)
                                      value)))
           (eval-all plugd da))]))

; BSL-da-all Symbol -> BSL-con-def
; produces the BSL-con-def whose name is x from da
(define (lookup-con-def da x)
  (cond [(empty? da) (error (con-not-found x))]
        [else (local ((define c-def (first da)))
                (if (and (cons? c-def)
                         (symbol=? (first c-def) x))
                    c-def
                    (lookup-con-def (rest da) x)))]))

; BSL-da-all Symbol -> BSL-fun-def
; produces the BSL-fun-def whose name is f from da
(define (lookup-fun-def da f)
  (cond [(empty? da) (error (fun-not-found f))]
        [else (local ((define f-def (first da)))
                (if (and (fun-def? f-def)
                         (symbol=? (fun-def-name f-def) f))
                    f-def
                    (lookup-fun-def (rest da) f)))]))

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
