#lang htdp/isl+

(require 2htdp/abstraction)

(define EURO->US-DOLLAR 1.06)

; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts
; based on an exchange rate EURO->US-DOLLAR
(check-expect (convert-euro '())
              '())
(check-expect (convert-euro '(1.06))
              '(1))
(check-expect (convert-euro '(5.3 1.06))
              '(5 1))
(define (convert-euro l)
  (for/list ([us$ l])
    (/ us$ EURO->US-DOLLAR)))
