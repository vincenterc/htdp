#lang htdp/isl+

(define-struct inex [mantissa sign exponent])
; An Inex is a structure:
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive)

(define EXP-OUT-OF-RANGE "The exponent is out of range")

; Inex Inex -> Inex
; multiplies i1 and i2
(check-expect
 (inex* (create-inex 2 1 4) (create-inex 8 1 10))
 (create-inex 16 1 14))
(check-expect
 (inex* (create-inex 20 1 1) (create-inex  5 1 4))
 (create-inex 10 1 6))
(check-expect
 (inex* (create-inex 27 -1 1) (create-inex  7 1 4))
 (create-inex 19 1 4))
(define (inex* i1 i2)
  (local ((define m1 (inex-mantissa i1))
          (define m2 (inex-mantissa i2))
          (define s1 (inex-sign i1))
          (define s2 (inex-sign i2))
          (define e1 (inex-exponent i1))
          (define e2 (inex-exponent i2))
		  ; exponent with sigh
          (define e-s-1 (* s1 e1))
          (define e-s-2 (* s2 e2))
          (define m (* m1 m2))
          (define e-s (+ e-s-1 e-s-2)))
    (get-inex m e-s)))

; Number Number -> Inex
; produces an Inex given a mantissa m and an exponent with a sign e-s
(check-error (get-inex 100 100) EXP-OUT-OF-RANGE)
(check-expect (get-inex 100 -100) (create-inex 10 -1 99))
(check-error (get-inex 50 100) EXP-OUT-OF-RANGE)
(check-error (get-inex 50 -100) EXP-OUT-OF-RANGE)
(check-expect (get-inex 117 50) (create-inex 12 1 51))
(check-expect (get-inex 111 -50) (create-inex 11 -1 49))
(check-error (get-inex 100 99) EXP-OUT-OF-RANGE)
(check-expect (get-inex 99 99) (create-inex 99 1 99))
(check-expect (get-inex 1 -99) (create-inex 1 -1 99))
(define (get-inex m e-s)
  (local ((define abs-e-s (abs e-s)))
    (cond [(> m 99) (get-inex (round (/ m 10)) (add1 e-s))]
          [(> abs-e-s 99) (error EXP-OUT-OF-RANGE)]
          [(<= abs-e-s 99)
           (create-inex m (if (>= e-s 0) 1 -1) abs-e-s)])))


; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
