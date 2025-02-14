#lang htdp/isl+

(require 2htdp/image)

(define FONT-SIZE 11)
(define FONT-COLOR "black")

; [List-of 1String] -> Image
; renders a string as an image for the editor
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right

(define 1S-WIDTH (image-width (editor-text '("a"))))

; [List-of 1String] Number -> Editor
; produces a Editor (make-editor p s) such that
; p and s make up ed0 and
; x is larger than the image of p and smaller than
; the image of p extended with th first 1String on s
(check-expect (split '("1" "2" "3")
                     (- 1S-WIDTH 1))
              (make-editor '() '("1" "2" "3")))
(check-expect (split '("1" "2" "3")
                     (+ 1S-WIDTH 2))
              (make-editor '("1") '("2" "3")))
(check-expect (split '("1" "2" "3")
                     (+ (* 1S-WIDTH 2) 1))
              (make-editor '("2" "1") '("3")))
(define (split ed0 x)
  (local (; [List-of 1String] [List-of 1String] -> Editor
          ; accumulator p is a list of items that ed lacks
          ; from ed0 in reverse order
          (define (split/a ed p)
            (cond
              [(empty? ed)
               (make-editor p '())]
              [(> (image-width
                   (editor-text
                    (cons (first ed) p))) x)
               (make-editor p ed)]
              [else
               (split/a (rest ed)
                        (cons (first ed) p))])))
    (split/a ed0 '())))
