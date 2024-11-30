#lang htdp/bsl

(require 2htdp/image)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

(define BACKGROUND (empty-scene 200 20))
(define CURSOR (rectangle 1 20 "solid" "red"))

; Editor -> Image
; produces an image that displays the text of the given e
; with a cursor
(check-expect (render (make-editor "hello" "world"))
              (overlay/align/offset
               "left" "center"
               (beside (text->image "hello")
                       CURSOR
                       (text->image "world"))
               -2 0
               BACKGROUND))
(define (render ed)
  (overlay/align/offset
   "left" "center"
   (beside (text->image (editor-pre ed))
           CURSOR
           (text->image (editor-post ed)))
   -2 0
   BACKGROUND))

; String -> Image
(define (text->image t)
  (text t 16 "black"))
