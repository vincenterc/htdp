#lang htdp/isl+

(define-struct add [left right])
; An Add is a structure:
;   (make-add BSL-expr BSL-expr)
; interpretation (make-add l r) represents (+ l r)

(define-struct mul [left right])
; A Mul is a structure:
;   (make-mul BSL-expr BSL-expr)
; interpretation (make-mul l r) represents (* l r)

; An BSL-expr is one of:
; - Number
; - Add
; - Mul

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

(define WRONG "Not BSL-expr")

; S-expr -> BSL-expr
(check-expect (parse 1) 1)
(check-expect (parse '(+ 10 -10))
              (make-add 10 -10))
(check-expect (parse '(+ (* 20 3) 33))
              (make-add (make-mul 20 3) 33))
(check-expect (parse '(+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9))))
              (make-add (make-mul 3.14 (make-mul 2 3))
                        (make-mul 3.14 (make-mul -1 -9))))
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))

; Atom -> BSL-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

; Atom -> Boolean
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
