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
(define (not-func-supported sy)
  (string-append (symbol->string sy)
                 ": "
                 "not a function supported!"))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'f) fun-f)
(check-expect (lookup-def da-fgh 'g) fun-g)
(check-expect (lookup-def da-fgh 'h) fun-h)
(check-error (lookup-def da-fgh 'i)
             (not-func-supported 'i))
(define (lookup-def da f)
  (local (; BSL-fun-def*
          (define lookup-result
            (filter (lambda (def) (symbol=? (fun-def-name def) f))
                    da)))
    (if (empty? lookup-result)
        (error (not-func-supported f))
        (first lookup-result))))
