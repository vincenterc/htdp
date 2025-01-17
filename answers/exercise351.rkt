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

; BSL-value is a Number

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

; S-expr -> BSL-value
; produces the value given s
(check-expect
 (interpreter-expr '(+ 10 -10)) 0)
(check-expect
 (interpreter-expr '(+ (* 20 3) 33)) 93)
(check-expect
 (interpreter-expr '(+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))) 47.1)
(check-error (interpreter-expr '(and #true #true)) WRONG)
(define (interpreter-expr s)
  (eval-expression (parse s)))

; BSL-expr -> BSL-value
; computes the value given the BSL-expr be
(define (eval-expression be)
  (cond [(number? be) be]
        [(add? be) (+ (eval-expression (add-left be))
                      (eval-expression (add-right be)))]
        [(mul? be) (* (eval-expression (mul-left be))
                      (eval-expression (mul-right be)))]))

; S-expr -> BSL-expr
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
