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
; replaces all variables in le0 with a natural number that
; represents how far away the declaring λ is
(check-expect (static-distance ex1)
              '(λ (x) 0))
(check-expect (static-distance ex2)
              '(λ (x) y))
(check-expect (static-distance ex3)
              '(λ (y) (λ (x) 1)))
(check-expect (static-distance ex4)
              '((λ (x) (0 0)) (λ (x) (0 0))))
(check-expect
 (static-distance '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
 '((λ (x) ((λ (y) (0 1)) 0)) (λ (z) 0)))
(define (static-distance le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (static-distance/a le declareds)
            (cond
              [(is-var? le)
               (local (; [Maybe N]
                       (define index
                         (index-of declareds le)))
                 (if (boolean? index) le index))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (static-distance/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (static-distance/a fun declareds)
                       (static-distance/a arg declareds)))])))
    (static-distance/a le0 '())))

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

; [X] [List-of X] X -> [Maybe N]
; finds the index of v in l0
; #false if v does not exist in l0
(check-expect (index-of '() 3) #false)
(check-expect (index-of '(1 2 3 4) 3) 2)
(check-expect (index-of '(1 2 3 4) 5) #false)
(define (index-of l0 v)
  (local (; [List-of X] N -> [Maybe N]
          ; accumulator a represents the difference in the number of
          ; elements between l0 and l
          (define (index-of/a l a)
            (cond
              [(empty? l) #false]
              [(equal? (first l) v) a]
              [else (index-of/a (rest l) (add1 a))])))
    (index-of/a l0 0)))
