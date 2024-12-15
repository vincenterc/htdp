#lang htdp/bsl

; A List-of-strings is one of
; - '()
; - (cons String List-of-strings)

; List-of-strings -> List-of-strings
; replaces all occurrences of "robot" in l with "r2d2"
(check-expect (subst-robot '())
              '())
(check-expect (subst-robot (cons "robot" '()))
              (cons "r2d2" '()))
(check-expect (subst-robot (cons "dog" '()))
              (cons "dog" '()))
(check-expect (subst-robot (cons "robot" (cons "dog" '())))
              (cons "r2d2" (cons "dog" '())))
(check-expect (subst-robot (cons "robot" (cons "robot" '())))
              (cons "r2d2" (cons "r2d2" '())))
(define (subst-robot l)
  (cond [(empty? l) '()]
        [else (if (string=? (first l) "robot")
                  (cons "r2d2" (subst-robot (rest l)))
                  (cons (first l) (subst-robot (rest l))))]))

; String String List-of-strings -> List-of-strings
; substitutes all occurrences of old in l with new
(check-expect (substitute "robot" "r2d2" '())
              '())
(check-expect (substitute "robot" "r2d2" (cons "robot" '()))
              (cons "r2d2" '()))
(check-expect (substitute "robot" "r2d2" (cons "dog" '()))
              (cons "dog" '()))
(check-expect (substitute "robot" "r2d2" (cons "robot" (cons "dog" '())))
              (cons "r2d2" (cons "dog" '())))
(check-expect (substitute "robot" "r2d2" (cons "robot" (cons "robot" '())))
              (cons "r2d2" (cons "r2d2" '())))
(define (substitute old new l)
  (cond [(empty? l) '()]
        [else (if (string=? (first l) old)
                  (cons new (subst-robot (rest l)))
                  (cons (first l) (subst-robot (rest l))))]))
