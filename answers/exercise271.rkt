#lang htdp/isl+

; String [List-of String] -> Boolean
; determines whether any of the names on the l
; are equal to or an extension of the name n0
(check-expect (find-name "john" '("emily" "james" "david")) #false)
(check-expect (find-name "john" '("james" "johnson" "david")) #true)
(define (find-name n0 l)
  (local (; String -> Boolean
          ; determines whether the name n are equal to
          ; or an extension of the name n0
          (define (=-or-extension-n0? n)
            (starts-with? n n0)))
    (ormap =-or-extension-n0? l)))

; [list-of String] -> Boolean
; checks all names on a list l that start with the letter "a".
(check-expect (all-start-with-a '("alice" "amelia" "adam")) #true)
(check-expect (all-start-with-a '("alice" "amelia" "daniel")) #false)
(define (all-start-with-a l)
  (local (; String -> Boolean
          ; determines whether the name n starts with the letter "a"
          (define (starts-with-a? n)
            (starts-with? n "a")))
    (andmap starts-with-a? l)))

; String String -> Boolean
; determines whether the string n starts with the string s
(check-expect (starts-with? "johnson" "john") #true)
(check-expect (starts-with? "james" "john") #false)
(define (starts-with? n s)
  (local ((define n-length (string-length n))
          (define s-length (string-length s)))
    (and (<= s-length n-length)
         (string=? (substring n 0 s-length) s))))

; Use andmap or (not ormap) can define a function that ensures that
; no name on some list exceeds a given width?