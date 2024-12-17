#lang htdp/bsl

(require 2htdp/batch-io)

; A List-of-strings is one of
; - '()
; - (cons String List-of-strings)

; An LN is one of:
; – '()
; – (cons Los LN)
; interpretation a list of lines, each is a list of Strings

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())

(define ln0 '())
(define ln1 (cons line0 (cons line1 '())))

; LN -> String
; converts a list of lines into a string
(check-expect (collapse ln0) "")
(check-expect (collapse ln1) "hello world\n")
(define (collapse ln)
  (cond [(empty? ln) ""]
        [else (string-append
               (los->string-space (first ln))
               (if (empty? (rest ln))
                   ""
                   (string-append "\n" (collapse (rest ln)))))]))

; List-of-strings -> String
; convert a list of strings into a string separated by blank spaces
(check-expect (los->string-space line0) "hello world")
(check-expect (los->string-space line1) "")
(define (los->string-space los)
  (cond [(empty? los) ""]
        [else (string-append
               (first los)
               (if (empty? (rest los))
                   ""
                   (string-append " " (los->string-space (rest los)))))]))

; Application
; (write-file "./files/ttt.dat"
;             (collapse (read-words/line "./files/ttt.txt")))
