#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

(define BACKGROUND-WIDTH 200)
(define BACKGROUND-HEIGHT 20)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

(define FONT-SIZE 16)
(define BORDER-WIDTH 2)
(define TEXT-WIDTH (- BACKGROUND-WIDTH BORDER-WIDTH BORDER-WIDTH))
(define CURSOR (rectangle 1 20 "solid" "red"))

; Editor -> Editor
; simulates a one-line editor
(define (run ed)
  (big-bang ed
    (on-draw render)
    (on-key edit)))

; Editor -> Image
; produces an image that displays the text of the given e
; with a cursor
(check-expect (render (make-editor "hello" "world"))
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
  (text t FONT-SIZE "black"))

; Editor -> Image
(define (text+cursor ed)
  (beside (text->image (editor-pre ed))
          CURSOR
          (text->image (editor-post ed))))

; Editor KeyEvent -> Editor
; produces an Editor, given ed and ke.
; The function adds a single-character ke to the end of the pre field of ed.
; If ke is "\b", it deletes the the last character of the pre filed of ed.
; It ignores "\b" and "\r" keys.
; If ke is "left", the last character of the pre field of ed is removed
; and added to the head of the post field of ed.
; If ke is "right", the first character of the post field of ed is removed
; and added to the tail of the pre field of ed.
(check-expect (edit (make-editor "12" "34") "q") (make-editor "12q" "34"))
(check-expect (edit (make-editor "12" "34") " ") (make-editor "12 " "34"))
(check-expect (edit (make-editor "" "1234") " ") (make-editor " " "1234"))
(check-expect (edit (make-editor "12" "34") "\b") (make-editor "1" "34"))
(check-expect (edit (make-editor "" "1234") "\b") (make-editor "" "1234"))
(check-expect (edit (make-editor "12" "34") "\t") (make-editor "12" "34"))
(check-expect (edit (make-editor "12" "34") "\r") (make-editor "12" "34"))
(check-expect (edit (make-editor "12" "34") "left") (make-editor "1" "234"))
(check-expect (edit (make-editor "" "1234") "left") (make-editor "" "1234"))
(check-expect (edit (make-editor "12" "34") "right") (make-editor "123" "4"))
(check-expect (edit (make-editor "1234" "") "right") (make-editor "1234" ""))
(check-expect (edit (make-editor "12" "34") "up") (make-editor "12" "34"))
(check-expect (edit (make-editor "12345678901234567890" "") "1") (make-editor "123456789012345678901" ""))
(check-expect (edit (make-editor "123456789012345678901" "") "2") (make-editor "123456789012345678901" ""))
(check-expect (edit (make-editor "1234567890123456789" "1") "0") (make-editor "12345678901234567890" "1"))
(check-expect (edit (make-editor "12345678901234567890" "2") "1") (make-editor "12345678901234567890" "2"))
(define (edit ed ke)
  (cond
    [(= (string-length ke) 1)
     (cond
       [(string=? ke "\t") ed]
       [(string=? ke "\r") ed]
       [(string=? ke "\b")
        (if (= (string-length (editor-pre ed)) 0)
            ed
            (make-editor (string-remove-last (editor-pre ed))
                         (editor-post ed)))]
       [else (if (> (image-width
                     (text+cursor
                      (add-1string ed ke)))
                    TEXT-WIDTH)
                 ed
                 (add-1string ed ke))])]
    [(string=? ke "left")
     (if (= (string-length (editor-pre ed)) 0)
         ed
         (make-editor (string-remove-last (editor-pre ed))
                      (string-append (string-last (editor-pre ed))
                                     (editor-post ed))))]
    [(string=? ke "right")
     (if (= (string-length (editor-post ed)) 0)
         ed
         (make-editor (string-append (editor-pre ed)
                                     (string-first (editor-post ed)))
                      (string-rest (editor-post ed))))]
    [else ed]))

; Editor 1String -> Editor
; adds 1s to the end of the pre field of ed
(check-expect (add-1string (make-editor "" "") "a")
              (make-editor "a" ""))
(check-expect (add-1string (make-editor "hell" "") "o")
              (make-editor "hello" ""))
(check-expect (add-1string (make-editor "" "23") "1")
              (make-editor "1" "23"))
(define (add-1string ed 1s)
  (make-editor
   (string-append (editor-pre ed) 1s)
   (editor-post ed)))

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
