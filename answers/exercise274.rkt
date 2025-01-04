#lang htdp/isl+

(define list-a (list "a"))
(define list-b (list "b"))
(define list-c (list "c"))
(define list-a-b (list "a" "b"))
(define list-b-c (list "b" "c"))
(define list-a-b-c (list "a" "b" "c"))

; [List-of 1String] -> [List-of [List-of 1String]]
; produces the list of all prefixes given l
(check-expect (prefixes list-a)
              (list list-a))
(check-expect (prefixes list-a-b)
              (list list-a list-a-b))
(check-expect (prefixes list-a-b-c)
              (list list-a list-a-b list-a-b-c))
(define (prefixes l)
  (local (; [1String] -> [List-of [List-of 1String]]
          ; produces a list of list of 1Strings given 1s and ll1s
          (define (f 1s ll1s)
            (local (; 1String -> [List-of 1String]
                    ; add 1s at the beginning of l1s
                    (define (add-1s-at-beginning l1s)
                      (cons 1s l1s)))
              (map add-1s-at-beginning (cons '() ll1s)))))
    (foldr f '() l)))

; [List-of 1String] -> [List-of [List-of 1String]]
; produces the list of all suffixes given l
(check-expect (suffixes list-a)
              (list list-a))
(check-expect (suffixes list-a-b)
              (list list-a-b list-b))
(check-expect (suffixes list-a-b-c)
              (list list-a-b-c list-b-c list-c))
(define (suffixes l)
  (local (; 1String -> [List-of [List-of 1String]]
          ; produces a list of list of 1Strings given 1s and lls
          (define (f 1s lls)
            (if (empty? lls)
                (list (list 1s))
                (cons (cons 1s (first lls)) lls))))
    (foldr f '() l)))
