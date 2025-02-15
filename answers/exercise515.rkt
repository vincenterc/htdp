#lang htdp/isl+

; A Lam is one of:
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

; Lam -> Lam
; replaces all symbols s in le0 with '*undeclared
; if they do not occur within the body of a λ
; expression whose parameter is s
(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) '(λ (x) *undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le '*undeclared)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; (undeclareds '(λ (*undeclared) ((λ (x) (x *undeclared)) y)))
; ==
; (list 'λ (list '*undeclared)
;       (list (list 'λ (list 'x) (list 'x '*undeclared))
;             '*undeclared))

; Lam -> Lam
; replaces a free occurrence of 'x with (list '*undeclared 'x)
; and a bound one 'y with (list '*declared 'y)
(check-expect (undeclareds.v2 ex1) '(λ (x) (*declared x)))
(check-expect (undeclareds.v2 ex2) '(λ (x) (*undeclared y)))
(check-expect (undeclareds.v2 ex3) '(λ (y) (λ (x) (*declared y))))
(define (undeclareds.v2 le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds)
                   (list '*declared le)
                   (list '*undeclared le))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; Lam -> Boolean
; determines whether expr is a variable
(define (is-var? expr)
  (symbol? expr))

; Lam -> Boolean
; determines whether expr is a lambda expression
(define (is-λ? expr)
  (and (cons? expr)
       (= (length expr) 3)
       (symbol=? (first expr) 'λ)
       (cons? (second expr))))

; Lam -> Boolean
; determines whether expr is an application
(define (is-app? expr)
  (and (cons? expr)
       (= (length expr) 2)))

; Lam -> Symbol
; extracts the parameter from a λ expression
(define (λ-para expr)
  (first (second expr)))

; Lam -> Lam
; extracts the body from a λ expression
(define (λ-body expr)
  (third expr))

; Lam -> Lam
; extracts the function from an application
(define (app-fun expr)
  (first expr))

; Lam -> Lam
; extracts the argument from an application
(define (app-arg expr)
  (second expr))
