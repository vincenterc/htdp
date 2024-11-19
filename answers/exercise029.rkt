#lang htdp/bsl

(define BASE-PRICE 5.0)
(define BASE-ATTENDEES 120)

(define PRICE-CHANGE 0.1)
(define ATTENDEE-CHANGE 15)

(define PERFORMANCE-COST 0)
(define ATTENDEE-COST 1.5)

(define (attendees ticket-price)
  (- BASE-ATTENDEES (* (- ticket-price BASE-PRICE) (/ ATTENDEE-CHANGE PRICE-CHANGE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ PERFORMANCE-COST (* ATTENDEE-COST (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

(define (profit2 price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (+ 0
        (* 1.50
           (+ 120
              (* (/ 15 0.1)
                 (- 5.0 price)))))))

(profit 3)   ; 630
(profit 4)   ; 675
(profit 5)   ; 420

(profit2 3)   ; 630
(profit2 4)   ; 675
(profit2 5)   ; 420