#lang htdp/isl+

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(check-expect (replace-eol-with '() '()) '())
(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with (cons 1 '()) '())
              (cons 1 '()))
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
(define (replace-eol-with front end)
  (cond [(empty? front) end]
        [(empty? end) front]
        [(cons? end)
         (cons (first front)
               (replace-eol-with (rest front) end))]))
; v4:
; (define (replace-eol-with front end)
;   (cond [(empty? front) end]
;         [(and (cons? front) (empty? end)) front]
;         [(and (cons? front) (cons? end))
;          (cons (first front)
;                (replace-eol-with (rest front) end))]))
; v3:
; (define (replace-eol-with front end)
;   (cond [(and (empty? front) (or (empty? end) (cons? end))) end]
;         [(and (cons? front) (empty? end)) front]
;         [(and (cons? front) (cons? end))
;          (cons (first front)
;                (replace-eol-with (rest front) end))]))
; v2:
; (define (replace-eol-with front end)
;   (cond [(or (and (empty? front) (empty? end))
;              (and (empty? front) (cons? end)))
;          end]
;         [(and (cons? front) (empty? end)) front]
;         [(and (cons? front) (cons? end))
;          (cons (first front)
;                (replace-eol-with (rest front) end))]))
; v1:
; (define (replace-eol-with front end)
;   (cond [(and (empty? front) (empty? end)) '()]
;         [(and (empty? front) (cons? end)) end]
;         [(and (cons? front) (empty? end)) front]
;         [(and (cons? front) (cons? end))
;          (cons (first front)
;                (replace-eol-with (rest front) end))]))
