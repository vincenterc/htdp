#lang htdp/isl+

(require htdp/dir)
(require 2htdp/abstraction)

; A File is a structure:
;   (make-file String N Date String)

; A Dir is a structure:
;   (make-dir String Dir* File*)

; A Dir* is [List-of Dir]

; A File* is [List-of File]

(define file-part1 (make-file "part1" 99 ""))
(define file-part2 (make-file "part2" 52 ""))
(define file-part3 (make-file "part3" 17 ""))
(define file-hang (make-file "hang" 8 ""))
(define file-draw (make-file "draw" 2 ""))
(define file-read!-1 (make-file "read!" 19 ""))
(define file-read!-2 (make-file "read!" 10 ""))

(define dir-Text
  (make-dir "Text" '() (list file-part1 file-part2 file-part3)))
(define dir-Code
  (make-dir "Code" '() (list file-hang file-draw)))
(define dir-Docs
  (make-dir "Docs" '() (list file-read!-1)))
(define dir-Libs
  (make-dir "Libs" (list dir-Code dir-Docs) '()))
(define dir-TS
  (make-dir "TS" (list dir-Text dir-Libs) (list file-read!-2)))

; A Path is [List-of String].
; interpretation directions into a directory tree

; Dir String -> [Maybe Path]
; produces the path of the file with name f if this file exists in d
; produces #false if not
(check-expect (find (make-dir "Empty" '() '()) "part1")
              #false)
(check-expect (find dir-Code "hang")
              (list "Code" "hang"))
(check-expect (find dir-Code "part2")
              #false)
(check-expect (find dir-Libs "read!")
              (list "Libs" "Docs" "read!"))
(check-expect (find dir-Libs "part3")
              #false)
(check-expect (find dir-TS "part3")
              (list "TS" "Text" "part3"))
(check-expect (find dir-TS "part4")
              #false)
(define (find d f)
  (local (; Boolean
          (define find-files-result
            (ormap (lambda (file) (string=? (file-name file) f))
                   (dir-files d))))
    (if find-files-result
        (list (dir-name d) f)
        (for/or ([dir (dir-dirs d)])
          (local (; [Maybe Path]
                  (define find-result (find dir f)))
            (if (not (boolean? find-result))
                (cons (dir-name d) find-result)
                #false))))))
; v3:
; (define (find d f)
;   (local (; File* -> Boolean
;           ; determines whether the file with name f exists in files
;           (define (find-files files)
;             (ormap (lambda (file) (string=? (file-name file) f)) files))
;           ; [Maybe Path]
;           (define find-dirs-result
;             (for/or ([dir (dir-dirs d)])
;               (local ((define find-result (find dir f)))
;                 (if (not (boolean? find-result))
;                     find-result
;                     #false)))))
;     (if (find-files (dir-files d))
;         (list (dir-name d) f)
;         (if (not (boolean? find-dirs-result))
;             (cons (dir-name d) find-dirs-result)
;             #false))))
; v2:
; (define (find d f)
;   (local (; File* -> Boolean
;           ; determines whether the file with name f exists in files
;           (define (find-files files)
;             (ormap (lambda (file) (string=? (file-name file) f)) files))
;           ; Dir* -> [Maybe Path]
;           ; produces the path of the file with name f
;           ; if this file exists in dirs
;           ; produces #false if not
;           (define (find-dirs dirs)
;             (for/or ([dir dirs])
;               (local ((define find-result (find dir f)))
;                 (if (not (boolean? find-result))
;                     find-result
;                     #false))))
;           ; [Maybe Path]
;           (define find-dirs-result (find-dirs (dir-dirs d))))
;     (if (find-files (dir-files d))
;         (list (dir-name d) f)
;         (if (not (boolean? find-dirs-result))
;             (cons (dir-name d) find-dirs-result)
;             #false))))
; v1:
; (define (find d f)
;   (local (; File* -> Boolean
;           ; determines whether the file with name f exists in files
;           (define (find-files files)
;             (cond [(empty? files) #false]
;                   [else (if (string=? (file-name (first files)) f)
;                             #true
;                             (find-files (rest files)))]))
;           ; Dir* -> [Maybe Path]
;           ; produces the path of the file with name f
;           ; if this file exists in dirs
;           ; produces #false if not
;           (define (find-dirs dirs)
;             (cond [(empty? dirs) #false]
;                   [else (local ((define find-result (find (first dirs) f)))
;                           (if (not (boolean? find-result))
;                               find-result
;                               (find-dirs (rest dirs))))]))
;           ; [Maybe Path]
;           (define find-dirs-result (find-dirs (dir-dirs d))))
;     (if (find-files (dir-files d))
;         (list (dir-name d) f)
;         (if (not (boolean? find-dirs-result))
;             (cons (dir-name d) find-dirs-result)
;             #false))))

; Dir String -> [List-of Path]
; produces the list of paths that lead to f in d
(check-expect (find-all (make-dir "Empty" '() '()) "part1")
              '())
(check-expect (find-all dir-Code "hang")
              (list (list "Code" "hang")))
(check-expect (find-all dir-Code "part2")
              '())
(check-expect (find-all dir-Libs "read!")
              (list (list "Libs" "Docs" "read!")))
(check-expect (find-all dir-Libs "part3")
              '())
(check-expect (find-all dir-TS "read!")
              (list (list "TS" "read!")
                    (list "TS" "Libs" "Docs" "read!")))
(check-expect (find-all dir-TS "part4")
              '())
(define (find-all d f)
  (local (; Boolean
          (define find-files-result
            (ormap (lambda (file) (string=? (file-name file) f))
                   (dir-files d)))
          ; [List-of Path]
          (define find-all-dirs-result
            (for*/list ([dir (dir-dirs d)] [p (find-all dir f)])
              (cons (dir-name d) p))))
    (if find-files-result
        (cons (list (dir-name d) f) find-all-dirs-result)
        find-all-dirs-result)))
; v1:
; (define (find-all d f)
;   (local (; Boolean
;           (define (find-files? files)
;             (ormap (lambda (file) (string=? (file-name file) f)) files))
;           ; Dir* -> [List-of Path]
;           ; produces the list of paths that lead to f in dirs
;           (define (find-all-dirs dirs)
;             (cond [(empty? dirs) '()]
;                   [else (append (find-all (first dirs) f)
;                                 (find-all-dirs (rest dirs)))]))
;           ; [List-of Path]
;           (define find-all-dirs-result
;             (local ((define result (find-all-dirs (dir-dirs d))))
;               (if (equal? result '())
;                   '()
;                   (map (lambda (p) (cons (dir-name d) p)) result)))))
;     (if (find-files? (dir-files d))
;         (cons (list (dir-name d) f) find-all-dirs-result)
;         find-all-dirs-result)))
