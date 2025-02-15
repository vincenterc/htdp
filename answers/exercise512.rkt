#lang htdp/isl+

; A Lam is one of:
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

; Lam -> Boolean
; determines whether expr is a variable
(check-expect (is-var? 'x) #true)
(check-expect (is-var? ex1) #false)
(check-expect (is-var? ex3) #false)
(check-expect (is-var? ex4) #false)
(define (is-var? expr)
  (symbol? expr))

; Lam -> Boolean
; determines whether expr is a lambda expression
(check-expect (is-λ? 'x) #false)
(check-expect (is-λ? ex1) #true)
(check-expect (is-λ? ex3) #true)
(check-expect (is-λ? ex4) #false)
(define (is-λ? expr)
  (and (cons? expr)
       (= (length expr) 3)
       (symbol=? (first expr) 'λ)
       (cons? (second expr))))

; Lam -> Boolean
; determines whether expr is an application
(check-expect (is-app? 'x) #false)
(check-expect (is-app? ex1) #false)
(check-expect (is-app? ex3) #false)
(check-expect (is-app? ex4) #true)
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

; Lam -> [List-of Symbol]
; produces the list of all symbols used as λ parameters
; in a λ term
(check-expect (declareds ex1) '(x))
(check-expect (declareds ex2) '(x))
(check-expect (declareds ex3) '(x y))
(check-expect (declareds ex4) '(x x))
(define (declareds expr0)
  (local (; Lam [List-of Symbol] -> [List-of Symbol]
          ; accumulator a is the list of λ parameters encountered
          ; on the path from the top of expr0 to the top of expr.
          (define (declareds/a expr a)
            (cond
              [(is-var? expr) a]
              [(is-λ? expr)
               (declareds/a (λ-body expr)
                            (cons (λ-para expr) a))]
              [(is-app? expr)
               (declareds/a (app-fun expr)
                            (declareds/a (app-arg expr) a))])))
    (declareds/a expr0 '())))
; (define (declareds expr)
;   (cond
;     [(is-var? expr) '()]
;     [(is-λ? expr)
;      (cons (λ-para expr) (declareds (λ-body expr)))]
;     [(is-app? expr)
;      (append (declareds (app-fun expr))
;              (declareds (app-arg expr)))]))
