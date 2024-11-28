#lang htdp/bsl

; A Letter is one of;
; - 1String "a" through "z" or
; - #false

(define-struct word [letter1 letter2 letter3])
; A Word is a structure:
;   (make-word Letter Letter Letter)
; interpretation a word consists of three Letter(s)

; Word Word -> Word
; produce a Word indicating the result of comparing two Word(s).
; If the content of the structure fields of w1 and w2 is the same,
; it is retained; otherwise, #false is placed in the field.
(check-expect (compare-word (make-word "a" "p" "t")
                            (make-word "a" "p" "t"))
              (make-word "a" "p" "t"))
(check-expect (compare-word (make-word "a" "p" "t")
                            (make-word "a" "p" "p"))
              (make-word "a" "p" #false))
(check-expect (compare-word (make-word #false "p" "t")
                            (make-word #false #false "t"))
              (make-word #false #false "t"))
(define (compare-word w1 w2)
  (make-word (same-letter? (word-letter1 w1)
                           (word-letter1 w2))
             (same-letter? (word-letter2 w1)
                           (word-letter2 w2))
             (same-letter? (word-letter3 w1)
                           (word-letter3 w2))))

; Letter Letter -> Letter
; compares two Letter(s).
; If l1 and l2 are the same, the function returns the same content,
; otherwise it returns #false
(check-expect (same-letter? "a" "a") "a")
(check-expect (same-letter? "a" "b") #false)
(check-expect (same-letter? "a" #false) #false)
(check-expect (same-letter? #false #false) #false)
(define (same-letter? l1 l2)
  (if (equal? l1 l2)
      l1
      #false))
