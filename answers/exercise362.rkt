#lang htdp/isl+

; An S-expr is one of:
; – Atom
; – SL

; An SL is one of:
; – '()
; – (cons S-expr SL)

; An Atom is one of:
; – Number
; – String
; – Symbol

(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name arg])
; A BSL-fun-expr is one of:
; – Number
; – Symbol
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-func Symbol BSL-fun-expr)

(define expr-1 '(area-of-circle 1))
(define expr-2 '(volume-of-10-cylinder 1))
(define expr-3 '(* 3 close-to-pi))
(define expr-4 '(+ 3 close-to-pi))
(define bsl-expr-1
  (make-fun 'area-of-circle 1))
(define bsl-expr-2
  (make-fun 'volume-of-10-cylinder 1))
(define bsl-expr-3
  (make-mul 3 'close-to-pi))
(define bsl-expr-4
  (make-add 3 'close-to-pi))

; A BSL-con-def is a list of two items:
;   (cons Symbol (cons Number '()))

(define-struct fun-def [name param body])
; A BSL-fun-def is a structure:
;   (make-fun-def Symbol Symbol BSL-fun-expr)

; A BSL-def is one of:
; - BSL-con-def
; - BSL-fun-def

(define def-close-to-pi
  '(define close-to-pi 3.14))
; BSL-con-def
(define con-close-to-pi
  (list 'close-to-pi 3.14))

(define def-area-of-circle
  '(define (area-of-circle r)
     (* close-to-pi (* r r))))
; BSL-fun-def
(define fun-area-of-circle
  (make-fun-def
   'area-of-circle
   'r
   (make-mul 'close-to-pi (make-mul 'r 'r))))

(define def-volume-of-10-cylinder
  '(define (volume-of-10-cylinder r)
     (* 10 (area-of-circle r))))
; BSL-fun-def
(define fun-volume-of-10-cylinder
  (make-fun-def
   'volume-of-10-cylinder
   'r
   (make-mul 10 (make-fun 'area-of-circle 'r))))

(define lod-1
  (list def-close-to-pi
        def-area-of-circle
        def-volume-of-10-cylinder))

(define EXPR-NOT-SUPPORT
  "The expression is not supported")
(define DEF-NOT-SUPPORT
  "The definition is not supported")
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

; S-expr SL -> number
; produces the value of expr given lod
(check-expect (interpreter expr-1 lod-1) 3.14)
(check-expect (interpreter expr-2 lod-1) 31.4)
(check-expect (interpreter expr-3 lod-1) 9.42)
(check-expect (interpreter expr-4 lod-1) 6.14)
(check-error
 (interpreter '(f 1) lod-1)
 (fun-not-found 'f))
(check-error
 (interpreter '(+ 3 x) lod-1)
 (con-not-found 'x))
(define (interpreter expr lod)
  (eval-all (parse expr)
            (map parse-def lod)))

; BSL-fun-expr BSL-da-all -> Number
; produces the value of ex given da
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

; S-expr -> BSL-fun-expr
(check-expect (parse expr-1) bsl-expr-1)
(check-expect (parse expr-2) bsl-expr-2)
(check-expect (parse expr-3) bsl-expr-3)
(check-expect (parse expr-4) bsl-expr-4)
(check-error (parse '(+ 1 1 1)) EXPR-NOT-SUPPORT)
(check-error (parse "hello") EXPR-NOT-SUPPORT)
(check-error (parse '(/ 1 2)) EXPR-NOT-SUPPORT)
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-fun-expr
(define (parse-sl s)
  (cond
    [(and (consists-of-2 s) (symbol? (first s)))
     (make-fun (first s) (parse (second s)))]
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error EXPR-NOT-SUPPORT)])]
    [else (error EXPR-NOT-SUPPORT)]))

; Atom -> BSL-fun-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(symbol? s) s]
    [(string? s) (error EXPR-NOT-SUPPORT)]))

; SL -> BSL-def
(check-expect
 (parse-def def-close-to-pi)
 con-close-to-pi)
(check-expect
 (parse-def def-area-of-circle)
 fun-area-of-circle)
(check-expect
 (parse-def def-volume-of-10-cylinder)
 fun-volume-of-10-cylinder)
(check-error
 (parse-def '(define (f x y) (+ x y)))
 DEF-NOT-SUPPORT)
(check-error
 (parse-def '(define (f x) x x))
 DEF-NOT-SUPPORT)
(define (parse-def d)
  (cond
    [(and (consists-of-3 d)
          (symbol? (first d))
          (symbol=? (first d) 'define))
     (cond
       [(symbol? (second d))
        (list (second d) (parse (third d)))]
       [(and (consists-of-2 (second d))
             (symbol? (first (second d)))
             (symbol? (second (second d))))
        (make-fun-def (first (second d))
                      (second (second d))
                      (parse (third d)))]
       [else (error DEF-NOT-SUPPORT)])]
    [else (error DEF-NOT-SUPPORT)]))

; SL -> Boolean
(define (consists-of-2 s)
  (and (cons? s) (cons? (rest s))
       (empty? (rest (rest s)))))

; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

; Any -> Boolean
; determines whether the given value a is an Atom
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
