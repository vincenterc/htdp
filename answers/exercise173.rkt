#lang htdp/bsl

(require 2htdp/batch-io)

; A List-of-strings is one of
; - '()
; - (cons String List-of-strings)

; An LN is one of:
; – '()
; – (cons Los LN)
; interpretation a list of lines, each is a list of Strings

(define FILES-DIR "./files")
(define FILE-PREFIX "no-articles-")

; String -> String
; removes all articles from a text file
(define (remove-articles-file file)
  (write-file
   (build-file-path
    (string-append FILE-PREFIX file))
   (collapse
    (remove-articles-ln
     (read-words/line (build-file-path file))))))

; String -> String
; builds a file path given a file name
(define (build-file-path file)
  (string-append FILES-DIR "/" file))

; ln -> ln
(define (remove-articles-ln ln)
  (cond [(empty? ln) '()]
        [else (cons (remove-articles-los (first ln))
                    (remove-articles-ln (rest ln)))]))

; list-of-strings -> list-of-strings
(define (remove-articles-los los)
  (remove "a" (remove "an" (remove "the" los))))

; LN -> String
; converts a list of lines into a string
(define (collapse ln)
  (cond [(empty? ln) ""]
        [else (string-append
               (los->string-space (first ln))
               (if (empty? (rest ln))
                   ""
                   (string-append "\n" (collapse (rest ln)))))]))

; List-of-strings -> String
; convert a list of strings into a string separated by blank spaces
(define (los->string-space los)
  (cond [(empty? los) ""]
        [else (string-append
               (first los)
               (if (empty? (rest los))
                   ""
                   (string-append " " (los->string-space (rest los)))))]))

; Application
; (remove-articles-file "ttt.txt")
