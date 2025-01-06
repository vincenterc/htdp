#lang htdp/isl+

; [X] X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(check-satisfied (find 2 (list 1 2 3))
                 (found? 2 (list 1 2 3)))
(check-satisfied (find 2 (list 1 2 2 3))
                 (found? 2 (list 1 2 2 3)))
(check-satisfied (find 5 (list 1 2 3))
                 (found? 5 (list 1 2 3)))
(check-satisfied (find "b" (list "a" "b" "c"))
                 (found? "b" (list "a" "b" "c")))
(check-satisfied (find "n" (list "a" "b" "c"))
                 (found? "n" (list "a" "b" "c")))
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

; [X] X [List-of X] -> [[Maybe List-of X] -> Boolean]
; is x not a member of k when l0 is #false
; does l0 starts with x
; is the length of l0 less than or equal to that of k
; is l0 a sublist of k
; is l0 is a FIRST sublist
(define (found? x k)
  (lambda (l0)
    (if (false? l0)
        (not (member? x k))
        (and (equal? (first l0) x)
             (<= (length l0) (length k))
             (local ((define index-x (- (length k) (length l0))))
               (and (equal? l0 (list-tail k index-x))
                    (not (member? x (take k index-x)))))))))

; [X] [List-of X] N -> [List-of X]
; returns the list after the first n0 elements of l0
(check-expect (list-tail '() 0)
              '())
(check-expect (list-tail '(1 2 3) 0)
              '(1 2 3))
(check-expect (list-tail '(1 2 3) 2)
              '(3))
(check-expect (list-tail '(1 2 3) 3)
              '())
(check-error
 (list-tail '(1 2 3) 4)
 "The length of the given list is less than the required number.")
(define (list-tail l0 n0)
  (if (> n0 (length l0))
      (error
       "The length of the given list is less than the required number.")
      (local ((define (list-tail-proper l n)
                (cond [(zero? n) l]
                      [else (list-tail-proper (rest l) (sub1 n))])))
        (list-tail-proper l0 n0))))

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
