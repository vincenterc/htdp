#lang htdp/isl+

(define-struct inex [mantissa sign exponent])
; An Inex is a structure:
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

(define EXP-OUT-OF-RANGE "The exponent is out of range")

; Inex Inex -> Inex
; adds two Inexs that have the same exponent
(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 2 1 0))
 (create-inex 3 1 0))
(check-expect
 (inex+ (create-inex 55 1 0) (create-inex 55 1 0))
 (create-inex 11 1 1))
(check-expect
 (inex+ (create-inex 56 1 0) (create-inex 56 1 0))
 (create-inex 11 1 1))
(check-error
 (inex+ (create-inex 56 1 99) (create-inex 56 1 99))
 EXP-OUT-OF-RANGE)
(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
 (create-inex 11 -1 1))
(define (inex+ inex1 inex2)
  (local ((define m1 (inex-mantissa inex1))
          (define m2 (inex-mantissa inex2))
          (define s1 (inex-sign inex1))
          (define s2 (inex-sign inex2))
          (define e1 (inex-exponent inex1))
          (define e2 (inex-exponent inex2))
          ; exponent with sigh
          (define e-s-1 (* s1 e1))
          (define e-s-2 (* s2 e2))
          (define e-s (min e-s-1 e-s-2))
          (define m (+ (* m1 (expt 10 (- e-s-1 e-s)))
                       (* m2 (expt 10 (- e-s-2 e-s))))))
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
