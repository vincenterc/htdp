#lang htdp/isl+

; A Set is a function:
;  [Number -> Boolean]
; interpretation consumes a element e and
; produces #true only if e belongs to the set

; Number -> Boolean
; checks whether e belongs to s
(define (in-set? e s)
  (s e))

; Number -> Boolean
; is n divisible by 10
(check-expect (divisible-10? 10) #true)
(check-expect (divisible-10? 11) #false)
(check-expect (divisible-10? 20) #true)
(check-expect (divisible-10? 32) #false)
(define (divisible-10? n)
  (= (modulo n 10) 0))

; Number -> Boolean
; is n odd
(define (is-odd? n)
  (and (integer? n)
       (odd? n)))

; Number -> boolean
; is n even
(define (is-even? n)
  (and (integer? n)
       (even? n)))

; Number -> Boolean] -> [Number -> Boolean]
; produces a set based on a predicate p
(check-expect (in-set? 2 (mk-set is-even?)) #true)
(check-expect (in-set? 1 (mk-set is-even?)) #false)
(check-expect (in-set? 1 (mk-set is-odd?)) #true)
(check-expect (in-set? 2 (mk-set is-odd?)) #false)
(check-expect (in-set? 10 (mk-set divisible-10?)) #true)
(check-expect (in-set? 55 (mk-set divisible-10?)) #false)
(define (mk-set p)
  (lambda (e)
    (p e)))

; Number Set -> Set
; adds an element to a set
(check-expect (in-set? 1 (add-element 1 (mk-set is-even?)))
              #true)
(check-expect (in-set? 2 (add-element 1 (mk-set is-even?)))
              #true)
(check-expect (in-set? 3 (add-element 1 (mk-set is-even?)))
              #false)
(define (add-element e s)
  (lambda (e0)
    (or (equal? e0 e)
        (s e0))))

; Set Set -> Set
; combines the elements of s1 and s2
(check-expect (in-set? 1 (union (mk-set is-even?)
                                (mk-set is-odd?)))
              #true)
(check-expect (in-set? 2 (union (mk-set is-even?)
                                (mk-set is-odd?)))
              #true)
(check-expect (in-set? 12 (union (mk-set is-even?)
                                 (mk-set divisible-10?)))
              #true)
(check-expect (in-set? 11 (union (mk-set is-odd?)
                                 (mk-set divisible-10?)))
              #true)
(check-expect (in-set? 12 (union (mk-set is-odd?)
                                 (mk-set divisible-10?)))
              #false)
(define (union s1 s2)
  (lambda (e)
    (or (s1 e) (s2 e))))

; Set Set -> Set
; collects all elements common to s1 and s2
(check-expect (in-set? 10 (intersect (mk-set is-even?)
                                     (mk-set divisible-10?)))
              #true)
(check-expect (in-set? 12 (intersect (mk-set is-even?)
                                     (mk-set divisible-10?)))
              #false)
(define (intersect s1 s2)
  (lambda (e)
    (and (s1 e) (s2 e))))
