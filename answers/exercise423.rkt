#lang htdp/isl+

; String N -> [List-of String]
; produces a list of string chunks of size n given s
(check-expect (partition "" 2) '())
(check-expect (partition "abcdefgh" 2)
              (list "ab" "cd" "ef" "gh"))
(check-expect (partition "abcdefg" 3)
              (list "abc" "def" "g"))
(define (partition s n)
  (cond [(string=? s "") '()]
        [else
         (local ((define end (min n (string-length s))))
           (cons (substring s 0 (min n end))
                 (partition (substring s end) n)))]))
