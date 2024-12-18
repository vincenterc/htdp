#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)
; Interpretation (make-editor pre post) means the letters in pre
; precede the cursor in reverse order and those in post succeed it

; An Lo1S is one of:
; – '()
; – (cons 1String Lo1S)

; Editor -> Editor
; moves the cursor position one 1String left,
; if possible
(check-expect
 (editor-lft (make-editor '() '()))
 (make-editor '() '()))
(check-expect
 (editor-lft
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))
 (make-editor (cons "d" '())
              (cons "e" (cons "f" (cons "g" '())))))
(check-expect
 (editor-lft
  (make-editor '() (cons "f" (cons "g" '()))))
 (make-editor '() (cons "f" (cons "g" '()))))
(check-expect
 (editor-lft
  (make-editor (cons "e" (cons "d" '()))
               '()))
 (make-editor (cons "d" '())
              (cons "e" '())))
(define (editor-lft ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed))
                   (cons (first (editor-pre ed))
                         (editor-post ed)))))

; Editor -> Editor
; moves the cursor position one 1String right,
; if possible
(check-expect
 (editor-rgt (make-editor '() '()))
 (make-editor '() '()))
(check-expect
 (editor-rgt
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))
 (make-editor (cons "f" (cons "e" (cons "d" '())))
              (cons "g" '())))
(check-expect
 (editor-rgt
  (make-editor '()
               (cons "f" (cons "g" '()))))
 (make-editor (cons "f" '())
              (cons "g" '())))
(check-expect
 (editor-rgt
  (make-editor (cons "e" (cons "d" '())) '()))
 (make-editor (cons "e" (cons "d" '())) '()))
(define (editor-rgt ed)
  (if (empty? (editor-post ed))
      ed
      (make-editor (cons (first (editor-post ed))
                         (editor-pre ed))
                   (rest (editor-post ed)))))

; Editor -> Editor
; deletes a 1String to the left of the cursor,
; if possible
(check-expect
 (editor-del (make-editor '() '()))
 (make-editor '() '()))
(check-expect
 (editor-del
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))
 (make-editor (cons "d" '())
              (cons "f" (cons "g" '()))))
(check-expect
 (editor-del
  (make-editor '() (cons "f" (cons "g" '()))))
 (make-editor '() (cons "f" (cons "g" '()))))
(check-expect
 (editor-del
  (make-editor (cons "e" (cons "d" '()))
               '()))
 (make-editor (cons "d" '())
              '()))
(define (editor-del ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed))
                   (editor-post ed))))
