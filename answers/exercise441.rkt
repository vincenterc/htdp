#lang htdp/isl+

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [else (local ((define pivot (first alon))
                  (define others (rest alon)))
            (append (quick-sort< (smaller-and-equals others pivot))
                    (list pivot)
                    (quick-sort< (largers others pivot))))]))

; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (filter (lambda (n0) (> n0 n)) alon))

; [List-of Number] Number -> [List-of Number]
(define (smaller-and-equals alon n)
  (filter (lambda (n0) (<= n0 n)) alon))

; (quick-sort< (list 10 6 8 9 14 12 3 11 14 16 2))
; (quick-sort< '(6 8 9 3 2))
; (quick-sort< '(3 2))
; (quick-sort< '(2))
; (quick-sort< '())
; (quick-sort< '(8 9))
; (quick-sort< '())
; (quick-sort< '(9))
; (quick-sort< '(14 12 11 14 16))
; (quick-sort< '(12 11 14))
; (quick-sort< '(11))
; (quick-sort< '(14))
; (quick-sort< '(16))
; 12 recursive applications of quick-sort<

; (append
;  (append
;   (append
;    '(2)
;    '(3)
;    '())
;   '(6)
;   (append
;    '()
;    '(8)
;    '(9)))
;  '(10)
;  (append
;   (append
;    '(11)
;    '(12)
;    '(14))
;   '(14)
;   '(16)))
; 5 recursive applications of append

; (quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 2 3 4 5 6 7 8 9 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 3 4 5 6 7 8 9 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 4 5 6 7 8 9 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 5 6 7 8 9 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 6 7 8 9 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 7 8 9 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 8 9 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 9 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 10 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 11 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 12 13 14))
; (quick-sort< '())
; (quick-sort< (list 13 14))
; (quick-sort< '())
; (quick-sort< (list 14))
; 26 recursive applications of quick-sort<

; (append
;  '()
;  '(1)
;  (append
;   '()
;   '(2)
;   (append
;    '()
;    '(3)
;    (append
;     '()
;     '(4)
;     (append
;      '()
;      '(5)
;      (append
;       '()
;       '(6)
;       (append
;        '()
;        '(7)
;        (append
;         '()
;         '(8)
;         (append
;          '()
;          '(9)
;          (append
;           '()
;           '(10)
;           (append
;            '()
;            '(11)
;            (append
;             '()
;             '(12)
;             (append
;              '()
;              '(13)
;              '(14))))))))))))))
; 12 recursive applications of append
