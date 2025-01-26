#lang htdp/isl+

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed

; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))

; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; [List-of Letter] HM-Word Letter -> HM-Word
; produces HM-Word with all "_" where the guess revealed a letter
; given the word to be guessed w, the current status s and
; the current guess g
(check-expect
 (compare-word '() '() "h")
 '())
(check-expect
 (compare-word '("h" "e" "l" "l" "o")
               '("_" "_" "_" "_" "_") "h")
 '("h" "_" "_" "_" "_"))
(check-expect
 (compare-word '("h" "e" "l" "l" "o")
               '("_" "_" "_" "_" "_") "l")
 '("_" "_" "l" "l" "_"))
(check-expect
 (compare-word '("h" "e" "l" "l" "o")
               '("_" "_" "_" "_" "_") "g")
 '("_" "_" "_" "_" "_"))
(define (compare-word w s g)
  (cond [(empty? w) '()]
        [else (if (string=? (first w) g)
                  (cons g (compare-word (rest w) (rest s) g))
                  (cons (first s) (compare-word (rest w) (rest s) g)))]))

; Application:
; (define LOCATION "./files/words")
; (define AS-LIST (read-lines LOCATION))
; (define SIZE (length AS-LIST))
; (play (list-ref AS-LIST (random SIZE)) 10)
