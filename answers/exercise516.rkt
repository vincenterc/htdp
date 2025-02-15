#lang htdp/isl+

; A Lam is one of:
; – a Symbol
; – L-expr
; – App
(define-struct l-expr [para body])
; A L-expr is a structure:
;   (make-l-expr Symbol Lam)
(define-struct app [fun arg])
; A App is a structure:
;   (make-app Lam Lam)

(define ex1 (make-l-expr 'x 'x))
(define ex2 (make-l-expr 'x 'y))
(define ex3 (make-l-expr 'y (make-l-expr 'x 'y)))

; Lam -> Lam
; replaces all symbols s in le0 with '*undeclared
; if they do not occur within the body of a λ
; expression whose parameter is s
(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2)
              (make-l-expr 'x '*undeclared))
(check-expect (undeclareds ex3) ex3)
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(symbol? le)
               (if (member? le declareds) le '*undeclared)]
              [(l-expr? le)
               (local ((define para (l-expr-para le))
                       (define body (l-expr-body le))
                       (define newd (cons para declareds)))
                 (make-l-expr para
                              (undeclareds/a body newd)))]
              [(app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (make-app (undeclareds/a fun declareds)
                           (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))
