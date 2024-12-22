#lang htdp/bsl+

(require 2htdp/itunes)

; A List-of-strings is one of:
; – '()
; – (cons String List-of-strings)

; A List-of-LTracks is one of:
; - '()
; - (cons LTracks List-of-LTracks)

(define date1
  (create-date 2002 7 17 3 55 14))
(define date2
  (create-date 2011 5 17 17 35 13))
(define date3
  (create-date 2002 7 17 3 55 42))
(define date4
  (create-date 2011 5 17 17 38 47))
(define date5
  (create-date 2002 7 18 1 2 25))
(define date6
  (create-date 2010 2 14 17 25 20))

(define album1 "A Day Without Rain")
(define album2 "Everybody Else Is Doing It, So Why Can't We?")

(define track1
  (create-track "Wild Child" "Enya" album1
                227996 2 date1 20 date2))
(define track2
  (create-track "Only Time" "Enya" album1
                218096 3 date3 18 date4))
(define track3
  (create-track "I Still Do" "The Cranberries" album2
                196780 1 date5 11 date6))

(define ltracks1 '())
(define ltracks2 (list track1 track3))
(define ltracks3 (list track1 track2 track3))

; LTracks -> List-of-LTracks
; produces a list of LTracks, one per album, given a list of tracks lt
(check-expect (select-albums ltracks1) '())
(check-expect (select-albums ltracks2)
              (list (list track1) (list track3)))
(check-expect (select-albums ltracks3)
              (list (list track1 track2) (list track3)))
(define (select-albums lt)
  (select-albums-by-albums
   (select-album-titles/unique lt) lt))

; List-of-strings LTracks -> List-of-LTracks
; produces a list of LTracks, one per album,
; given a list of unique album titles as and a list of tracks lt
(check-expect
 (select-albums-by-albums '() ltracks1)
 '())
(check-expect
 (select-albums-by-albums (list album1) ltracks1)
 (list '()))
(check-expect
 (select-albums-by-albums (list album1) ltracks2)
 (list (list track1)))
(check-expect
 (select-albums-by-albums (list album1) ltracks3)
 (list (list track1 track2)))
(check-expect
 (select-albums-by-albums (list album1 album2) ltracks3)
 (list (list track1 track2) (list track3)))
(define (select-albums-by-albums as lt)
  (cond [(empty? as) '()]
        [else (cons (select-album (first as) lt)
                    (select-albums-by-albums (rest as) lt))]))

; LTracks -> List-of-strings
; produces a list of unique album titles given a list of tracks lt
(check-expect (select-album-titles/unique ltracks1) '())
(check-expect (select-album-titles/unique ltracks2)
              (list album1 album2))
(check-expect (select-album-titles/unique ltracks3)
              (list album1 album2))
(define (select-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))

; LTracks -> List-of-strings
; produces the list of album titles given a list of tracks lt
(check-expect (select-all-album-titles ltracks1) '())
(check-expect (select-all-album-titles ltracks2)
              (list album1 album2))
(check-expect (select-all-album-titles ltracks3)
              (list album1 album1 album2))
(define (select-all-album-titles lt)
  (cond [(empty? lt) '()]
        [else
         (cons (track-album (first lt))
               (select-all-album-titles (rest lt)))]))

; List-of-strings -> List-of-strings
; produces the list of strings that contains every String
; from the given list los exactly once
(check-expect (create-set '()) '())
(check-expect (create-set (list "a"))
              (list "a"))
(check-expect (create-set (list "a" "b"))
              (list "a" "b"))
(check-expect (create-set (list "a" "a" "b"))
              (list "a" "b"))
(define (create-set los)
  (cond [(empty? los) '()]
        [else (cons-or-not (first los) (create-set (rest los)))]))

; String -> List-of-strings
; constructs a new list only when s is not a member of los,
; otherwise returns los
(check-expect (cons-or-not "a" (list "a" "b"))
              (list "a" "b"))
(check-expect (cons-or-not "a" (list "b" "c"))
              (list "a" "b" "c"))
(define (cons-or-not s los)
  (if (member? s los)
      los
      (cons s los)))

; String LTracks -> LTracks
; produces the list of tracks that belong to the given album a
; from lt
(check-expect (select-album album1 ltracks1) '())
(check-expect (select-album album1 ltracks2)
              (list track1))
(check-expect (select-album album1 ltracks3)
              (list track1 track2))
(define (select-album a lt)
  (cond [(empty? lt) '()]
        [else (if (string=? (track-album (first lt)) a)
                  (cons (first lt) (select-album a (rest lt)))
                  (select-album a (rest lt)))]))

; Application
; (define ITUNES-LOCATION "./files/itunes.xml")
; ; LTracks
; (define itunes-tracks
;   (read-itunes-as-tracks ITUNES-LOCATION))

; (length (select-albums itunes-tracks))
