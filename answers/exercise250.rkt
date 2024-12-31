#lang htdp/isl

; Number -> [List-of Number]
; tabulates sin between n
; and 0 (incl.) in a list
(check-within (tab-sin 0)
              (list 0) 0.0001)
(check-within (tab-sin 1)
              (list 0.841 0) 0.001)
(check-within (tab-sin 2)
              (list 0.909 0.841 0) 0.001)
; (define (tab-sin n)
;   (cond
;     [(= n 0) (list (sin 0))]
;     [else
;      (cons
;       (sin n)
;       (tab-sin (sub1 n)))]))
(define (tab-sin n)
  (tabulate n sin))

; Number -> [List-of Number]
; tabulates sqrt between n
; and 0 (incl.) in a list
(check-within (tab-sqrt 0)
              (list 0) 0.0001)
(check-within (tab-sqrt 1)
              (list 1 0) 0.001)
(check-within (tab-sqrt 2)
              (list 1.414 1 0) 0.001)
; (define (tab-sqrt n)
;   (cond
;     [(= n 0) (list (sqrt 0))]
;     [else
;      (cons
;       (sqrt n)
;       (tab-sqrt (sub1 n)))]))
(define (tab-sqrt n)
  (tabulate n sqrt))

(define (tabulate n f)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate (sub1 n) f))]))

(define (tab-sqr n)
  (tabulate n sqr))

(define (tab-tan n)
  (tabulate n tan))
