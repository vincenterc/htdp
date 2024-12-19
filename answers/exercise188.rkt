#lang htdp/bsl+

(define-struct email [from date message])
; An Email is a structure:
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m
; sent by f, d seconds after the beginning of time

; List-of-emails is one of:
; - '()
; - (cons Email List-of-emails)

(define e1 (make-email "a1" 100 "hello"))
(define e2 (make-email "a2" 150 "world"))
(define e3 (make-email "b1" 200 "htdp"))
(define e4 (make-email "b2" 250 "bsl"))

; List-of-emails -> List-of-emails
; sorts a list of emails by date
(check-expect (sort-email/date '()) '())
(check-expect (sort-email/date (list e2 e3 e1 e4))
              (list e4 e3 e2 e1))
(define (sort-email/date l)
  (cond [(empty? l) '()]
        [else (insert-email/date (first l) (sort-email/date (rest l)))]))

; Email List-of-emails -> List-of-emails
; inserts a email into the sorted list of email
(define (insert-email/date e l)
  (cond [(empty? l) (cons e '())]
        [else (if (> (email-date e)
                     (email-date (first l)))
                  (cons e l)
                  (cons (first l) (insert-email/date e (rest l))))]))

; List-of-emails -> List-of-emails
; sorts a list of emails by name
(check-expect (sort-email/name '()) '())
(check-expect (sort-email/name (list e2 e3 e1 e4))
              (list e1 e2 e3 e4))
(define (sort-email/name l)
  (cond [(empty? l) '()]
        [else (insert-email/name (first l) (sort-email/name (rest l)))]))

; Email List-of-emails -> List-of-emails
; inserts a email into the sorted list of email
(define (insert-email/name e l)
  (cond [(empty? l) (cons e '())]
        [else (if (string<? (email-from e)
                            (email-from (first l)))
                  (cons e l)
                  (cons (first l) (insert-email/name e (rest l))))]))
