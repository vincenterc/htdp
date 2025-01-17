#lang htdp/isl+

(define-struct bsl-and [left right])
; An And is a struct:
;   (make-and Boolean Boolean)
; interpretation (make-and l r) represents (and l r)

(define-struct bsl-or [left right])
; A Or is a struct:
;   (make-and Boolean Boolean)
; interpretation (make-or l r) represents (or l r)

(define-struct bsl-not [bool])
; A Not is a struct:
;   (make-not Boolean)
; interpretation (make-and b) represents (not b)

; BSL-bool is one of:
; - #true
; - #false
; - And
; - Or
; - Not

; BSL-value is a Boolean

; BSL-bool -> BSL-value
; computes the value given the BSL-bool bb
(check-expect
 (eval-bool-expression #false) #false)
(check-expect
 (eval-bool-expression #true) #true)
(check-expect
 (eval-bool-expression (make-bsl-and #true #true)) #true)
(check-expect
 (eval-bool-expression (make-bsl-and #true #false)) #false)
(check-expect
 (eval-bool-expression (make-bsl-and #false #false)) #false)
(check-expect
 (eval-bool-expression (make-bsl-or #true #true)) #true)
(check-expect
 (eval-bool-expression (make-bsl-or #true #false)) #true)
(check-expect
 (eval-bool-expression (make-bsl-or #false #false)) #false)
(check-expect
 (eval-bool-expression (make-bsl-not #false)) #true)
(check-expect
 (eval-bool-expression (make-bsl-not #true)) #false)
(define (eval-bool-expression bb)
  (cond [(boolean? bb) bb]
        [(bsl-and? bb) (and (bsl-and-left bb) (bsl-and-right bb))]
        [(bsl-or? bb) (or (bsl-or-left bb) (bsl-or-right bb))]
        [(bsl-not? bb) (not (bsl-not-bool bb))]))
