#lang htdp/bsl

(require 2htdp/batch-io)

; An LN is one of:
; – '()
; – (cons Los LN)
; interpretation a list of lines, each is a list of Strings

; A List-of-strings is one of
; - '()
; - (cons String List-of-strings)

; A List-of-1Strings is one of
; - '()
; - (cons 1String List-of-1Strings)

(define FILES-DIR "./files")
(define FILE-PREFIX "encoded-")

(define lol0 (cons "h" (cons "e" (cons "l" (cons "l" (cons "o" '()))))))
(define lol1 (cons "w" (cons "o" (cons "r" (cons "l" (cons "d" '()))))))

(define lol0-encoded
  (cons "104" (cons "101" (cons "108" (cons "108" (cons "111" '()))))))
(define lol1-encoded
  (cons "119" (cons "111" (cons "114" (cons "108" (cons "100" '()))))))

(define s0 "hello")
(define s1 "world")

(define s0-encoded "104101108108111")
(define s1-encoded "119111114108100")

(define line0 (cons s0 (cons s1 '())))

; String -> LN
; encodes a text file numerically
(define (encode-file file)
  (write-file
   (build-file-path (string-append FILE-PREFIX file))
   (collapse (encode-ln (read-words/line (build-file-path file))))))

; String -> String
; builds a file path given a file name
(define (build-file-path file)
  (string-append FILES-DIR "/" file))

; LN -> LN
; encodes a list of lines
(define (encode-ln ln)
  (cond [(empty? ln) '()]
        [else (cons (encode-los (first ln))
                    (encode-ln (rest ln)))]))

; List-of-strings -> List-of-string
; encodes a list of strings
(check-expect (encode-los line0)
              (cons s0-encoded (cons s1-encoded '())))
(define (encode-los los)
  (cond [(empty? los) '()]
        [else (cons (encode-string (first los))
                    (encode-los (rest los)))]))

; String -> String
; encodes a string
(check-expect (encode-string s0) s0-encoded)
(check-expect (encode-string s1) s1-encoded)
(define (encode-string str)
  (los->string (encode-lol (explode str))))

; List-of-1Strings -> List-of-strings
; encodes a list of letters
(check-expect (encode-lol lol0) lol0-encoded)
(check-expect (encode-lol lol1) lol1-encoded)
(define (encode-lol lol)
  (cond [(empty? lol) '()]
        [else (cons (encode-letter (first lol))
                    (encode-lol (rest lol)))]))

; List-of-strings -> String
; convert a list of strings into a string
(check-expect (los->string lol0) s0)
(check-expect (los->string lol1) s1)
(define (los->string los)
  (cond [(empty? los) ""]
        [else (string-append (first los)
                             (los->string (rest los)))]))

; 1String -> String
; converts the given 1String to a 3-letter numeric String
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))

; 1String -> String
; converts the given 1String into a String
(check-expect (code1 "z") "122")
(define (code1 c)
  (number->string (string->int c)))

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
; (encode-file "ttt.txt")
