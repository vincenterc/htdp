#lang htdp/isl+

; A Pair is a list of two items:
;   (list Symbol Number)

; [List-of Symbol] [List-of Number] -> [List-of Pair]
; produces all possible ordered pairs of symbol and numbers
; given losy and lon
(check-expect
 (cross '() '())
 '())
(check-expect
 (cross '(a b c) '())
 '())
(check-expect
 (cross '() '(1 2))
 '())
(check-expect
 (cross '(a b c) '(1 2))
 '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(define (cross losy lon)
  (local (; Symbol [List-of Number] -> [List-of Pair]
          ; produces pairs of sy and each number in lon1
          (define (process-lon sy lon1)
            (cond [(empty? lon1) '()]
                  [else (cons (list sy (first lon1))
                              (process-lon sy (rest lon1)))])))
    (cond [(empty? losy) '()]
          [else (append (process-lon (first losy) lon)
                        (cross (rest losy) lon))])))
