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

; BSL-da-all Symbol -> BSL-con-def
; produces the BSL-con-def whose name is x from da
(check-expect (lookup-con-def da-all 'close-to-pi)
              con-close-to-pi)
(check-error (lookup-con-def da-all 'area-of-circle)
             (con-not-found 'area-of-circle))
(define (lookup-con-def da x)
  (cond [(empty? da) (error (con-not-found x))]
        [else (local ((define c-def (first da)))
                (if (and (cons? c-def)
                         (symbol=? (first c-def) x))
                    c-def
                    (lookup-con-def (rest da) x)))]))

; BSL-da-all Symbol -> BSL-fun-def
; produces the BSL-fun-def whose name is f from da
(check-expect (lookup-fun-def da-all 'area-of-circle)
             fun-area-of-circle)
(check-expect (lookup-fun-def da-all 'volume-of-10-cylinder)
             fun-volume-of-10-cylinder)
(check-error (lookup-fun-def da-all 'close-to-pi)
             (fun-not-found 'close-to-pi))
(define (lookup-fun-def da f)
  (cond [(empty? da) (error (fun-not-found f))]
        [else (local ((define f-def (first da)))
                (if (and (fun-def? f-def)
                         (symbol=? (fun-def-name f-def) f))
                    f-def
                    (lookup-fun-def (rest da) f)))]))
