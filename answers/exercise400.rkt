#lang htdp/isl+

; A Base is on of:
; - 'a
; - 'c
; - 'g
; - 't
; A DNA is [List-of Base]

; DNA DNA -> Boolean
; determines whether the pattern p is identical to the initial part of
; the search string ss
(check-expect (DNAprefix '() '()) #true)
(check-expect (DNAprefix '() '(a c g)) #true)
(check-expect (DNAprefix '(a) '()) #false)
(check-expect (DNAprefix '(a c g) '(a c g t)) #true)
(check-expect (DNAprefix '(a t g) '(a c g t)) #false)
(define (DNAprefix p ss)
  (cond [(empty? p) #true]
        [(empty? ss) #false]
        [(cons? ss)
         (and (symbol=? (first p) (first ss))
              (DNAprefix (rest p) (rest ss)))]))

(define IDENTICAL "The two lists are identical!")

; DNA DNA -> Boolean
; produces the first item in the search string ss beyond the pattern p
; produces #false if p does not match the beginning of ss
; signals an error if the lists are identical and there is no DNA letter
; beyond the pattern
(check-error (DNAdelta '() '()) IDENTICAL)
(check-expect (DNAdelta '() '(a c g)) 'a)
(check-expect (DNAdelta '(a) '()) #false)
(check-expect (DNAdelta '(a c g) '(a c g t)) 't)
(check-expect (DNAdelta '(a t g) '(a c g t)) #false)
(define (DNAdelta p ss)
  (cond [(and (empty? p) (empty? ss)) (error IDENTICAL)]
        [(and (empty? p) (cons? ss)) (first ss)]
        [(empty? ss) #false]
        [(cons? ss)
         (if (symbol=? (first p) (first ss))
             (DNAdelta (rest p) (rest ss))
             #false)]))
; v1:
; (define (DNAdelta p ss)
;   (cond [(and (empty? p) (empty? ss)) (error IDENTICAL)]
;         [(and (empty? p) (cons? ss)) (first ss)]
;         [(and (cons? p) (empty? ss)) #false]
;         [(and (cons? p) (cons? ss))
;          (if (symbol=? (first p) (first ss))
;              (DNAdelta (rest p) (rest ss))
;              #false)]))