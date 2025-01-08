#lang htdp/isl+

(require 2htdp/abstraction)

; String [List-of String] -> [Maybe String]
; retrieves the first name on l that is equal to,
; or an extension of n0
(check-expect (find-name "john" '("emily" "james" "david")) #false)
(check-expect (find-name "john" '("james" "david" "john")) "john")
(check-expect (find-name "john" '("james" "johnson" "david")) "johnson")
(define (find-name n0 l)
  (for/or ([n l])
    (if (starts-with? n n0) n #false)))

; String String -> Boolean
; determines whether the string n starts with the string s
(check-expect (starts-with? "johnson" "john") #true)
(check-expect (starts-with? "james" "john") #false)
(define (starts-with? n s)
  (local ((define n-length (string-length n))
          (define s-length (string-length s)))
    (and (<= s-length n-length)
         (string=? (substring n 0 s-length) s))))

; N [List-of String] -> Boolean
; ensures that no name on l exceeds some given width w
(check-expect (not-exceed-length? 6 '("emily" "james" "david")) #true)
(check-expect (not-exceed-length? 4 '("emily" "james" "david")) #false)
(define (not-exceed-length? w l)
  (for/and ([n l]) (< (string-length n) w)))
