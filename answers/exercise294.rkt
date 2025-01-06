#lang htdp/isl+

; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(check-satisfied (index 2 (list 1 2 3))
                 (is-index? 2 (list 1 2 3)))
(check-satisfied (index 2 (list 1 2 2 3))
                 (is-index? 2 (list 1 2 2 3)))
(check-satisfied (index 5 (list 1 2 3))
                 (is-index? 5 (list 1 2 3)))
(check-satisfied (index "b" (list "a" "b" "c"))
                 (is-index? "b" (list "a" "b" "c")))
(check-satisfied (index "n" (list "a" "b" "c"))
                 (is-index? "n" (list "a" "b" "c")))
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; [X] X [List-of X] -> [[Maybe N] -> Boolean]
; is x not a member of k if i0 is #false
; is i0 less than the length of k
; dose the i0-th item of the k equal x
; is x not a member of first i0 elements of k
(define (is-index? x k)
  (lambda (i0)
    (if (false? i0)
        (not (member? x k))
        (and (< i0 (length k))
             (equal? (list-ref k i0) x)
             (not (member? x (take k i0)))))))

; [X] [List-of X] N -> [List-of X]
; returns a fresh list whose elements are the first n0 elements of l0
(check-expect (take '() 0)
              '())
(check-expect (take '(1 2 3) 0)
              '())
(check-expect (take '(1 2 3) 2)
              '(1 2))
(check-error
 (take '(1 2 3) 4)
 "The length of the given list is less than the required number.")
(define (take l0 n0)
  (if (> n0 (length l0))
      (error
       "The length of the given list is less than the required number.")
      (local ((define (take-proper l n)
                (cond [(zero? n) '()]
                      [else (cons (first l)
                                  (take (rest l) (sub1 n)))])))
        (take-proper l0 n0))))
