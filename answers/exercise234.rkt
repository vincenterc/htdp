#lang htdp/bsl+

(require 2htdp/web-io)

; A Rank is a list of two items:
;   (list Number String)

; List-of-ranks is one of:
; '()
; (cons Rank List-of-ranks)

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; List-of-strings -> List-of-ranks
; creates a list of ranks from los
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> List-of-ranks
; adds a rank number to each item in los
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; List-of-ranks -> ... nested list ...
(define (make-rows lor)
  (cond [(empty? lor) '()]
        [else (cons (make-row (first lor))
                    (make-rows (rest lor)))]))

; Rank -> ... nested list ...
; creates a row for an HTML table from r
(define (make-row r)
  `(tr ((align "center"))
       (td ,(number->string (first r)))
       (td ,(second r))))

; List-of-strings -> ... nested list ...
(define (make-rankings los)
  `(table ((border "1"))
          ,@(make-rows (ranking los))))

; Application
; (make-rankings one-list)
; (show-in-browser (make-rankings one-list))
