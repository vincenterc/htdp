#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [text cursor])
; An Editor is a structure:
;   (make-editor String Number)
; interpretation (make-editor t c) describes an editor
; whose visible text is t with the cursor displayed at c,
; the number of characters between
; the first character (counting from the left) and the cursor

(define BACKGROUND-WIDTH 200)
(define BACKGROUND-HEIGHT 20)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

(define FONT-SIZE 16)
(define FONT-COLOR "black")
(define BORDER-WIDTH 2)
(define TEXT-WIDTH (- BACKGROUND-WIDTH BORDER-WIDTH BORDER-WIDTH))
(define CURSOR (rectangle 1 20 "solid" "red"))

; Editor -> Image
; simulates a one-line editor
(define (run ed)
  (big-bang ed
    (on-draw render)
    (on-key edit)))

; Editor -> Image
; produces an image that displays the text of the given e
; with a cursor
(check-expect (render (make-editor "helloworld" 5))
              (overlay/align/offset
               "left" "center"
               (beside (text->image "hello")
                       CURSOR
                       (text->image "world"))
               (- BORDER-WIDTH) 0
               BACKGROUND))
(define (render ed)
  (overlay/align/offset
   "left" "center"
   (text+cursor ed)
   (- BORDER-WIDTH) 0
   BACKGROUND))

; String -> Image
(define (text->image t)
  (text t FONT-SIZE FONT-COLOR))

; Editor -> Image
(define (text+cursor ed)
  (beside
   (text->image (substring (editor-text ed) 0 (editor-cursor ed)))
   CURSOR
   (text->image (substring (editor-text ed) (editor-cursor ed)))))

; Editor KeyEvent -> Editor
; produces an Editor, given ed and ke.
; The function adds a single-character ke to the end of the pre field of ed.
; If ke is "\b", it deletes the the last character of the pre filed of ed.
; It ignores "\b" and "\r" keys.
; If ke is "left", the last character of the pre field of ed is removed
; and added to the head of the post field of ed.
; If ke is "right", the first character of the post field of ed is removed
; and added to the tail of the pre field of ed.
(check-expect (edit (make-editor "1234" 2) "q") (make-editor "12q34" 3))
(check-expect (edit (make-editor "1234" 2) " ") (make-editor "12 34" 3))
(check-expect (edit (make-editor "1234" 0) " ") (make-editor " 1234" 1))
(check-expect (edit (make-editor "1234" 2) "\b") (make-editor "134" 1))
(check-expect (edit (make-editor "1234" 0) "\b") (make-editor "1234" 0))
(check-expect (edit (make-editor "1234" 2) "\t") (make-editor "1234" 2))
(check-expect (edit (make-editor "1234" 2) "\r") (make-editor "1234" 2))
(check-expect (edit (make-editor "1234" 2) "left") (make-editor "1234" 1))
(check-expect (edit (make-editor "1234" 0) "left") (make-editor "1234" 0))
(check-expect (edit (make-editor "1234" 2) "right") (make-editor "1234" 3))
(check-expect (edit (make-editor "1234" 4) "right") (make-editor "1234" 4))
(check-expect (edit (make-editor "1234" 2) "up") (make-editor "1234" 2))
(check-expect (edit (make-editor "12345678901234567890" 20) "1") (make-editor "123456789012345678901" 21))
(check-expect (edit (make-editor "123456789012345678901" 21) "2") (make-editor "123456789012345678901" 21))
(check-expect (edit (make-editor "12345678901234567891" 19) "0") (make-editor "123456789012345678901" 20))
(check-expect (edit (make-editor "123456789012345678902" 20) "1") (make-editor "123456789012345678902" 20))
(define (edit ed ke)
  (cond
    [(= (string-length ke) 1)
     (cond
       [(string=? ke "\t") ed]
       [(string=? ke "\r") ed]
       [(string=? ke "\b")
        (if (or (= (string-length (editor-text ed)) 0)
                (= (editor-cursor ed) 0))
            ed
            (make-editor
             (string-remove-at (editor-text ed) (editor-cursor ed))
             (- (editor-cursor ed) 1)))]
       [else (if (> (image-width
                     (text+cursor
                      (add-1string ed ke)))
                    TEXT-WIDTH)
                 ed
                 (add-1string ed ke))])]
    [(string=? ke "left")
     (if (or (= (string-length (editor-text ed)) 0)
             (= (editor-cursor ed) 0))
         ed
         (make-editor (editor-text ed)
                      (- (editor-cursor ed) 1)))]
    [(string=? ke "right")
     (if (or (= (string-length (editor-text ed)) 0)
             (= (editor-cursor ed) (string-length (editor-text ed))))
         ed
         (make-editor (editor-text ed)
                      (+ (editor-cursor ed) 1)))]
    [else ed]))

; Editor 1String -> Editor
; adds 1s to the end of the pre field of ed
(check-expect (add-1string (make-editor "" 0) "a")
              (make-editor "a" 1))
(check-expect (add-1string (make-editor "ello" 0) "h")
              (make-editor "hello" 1))
(check-expect (add-1string (make-editor "hllo" 1) "e")
              (make-editor "hello" 2))
(check-expect (add-1string (make-editor "hell" 4) "o")
              (make-editor "hello" 5))
(define (add-1string ed 1s)
  (make-editor
   (string-append
    (substring (editor-text ed) 0 (editor-cursor ed ))
    1s
    (substring (editor-text ed) (editor-cursor ed )))
   (+ (editor-cursor ed) 1)))

; String -> 1String
; extract the first character from str
(check-expect (string-first "hello") "h")
(define (string-first str)
  (substring str 0 1))

; String -> 1String
; extract the last character from non-empty str
(check-expect (string-last "hello") "o")
(define (string-last str)
  (substring str (- (string-length str) 1)))

; String -> String
; produce a string from str with the first character removed
(check-expect (string-rest "hello") "ello")
(define (string-rest str)
  (substring str 1))

; String -> String
; produce a string from str with the last character removed
(check-expect (string-remove-last "hello") "hell")
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))

; String Number -> String
; produce a string from str with the nth character removed
; (counting from the left)
(check-expect (string-remove-at "hello" 1) "ello")
(check-expect (string-remove-at "hello" 3) "helo")
(check-expect (string-remove-at "hello" 5) "hell")
(define (string-remove-at str n)
  (string-append (substring str 0 (- n 1))
                 (substring str n)))
