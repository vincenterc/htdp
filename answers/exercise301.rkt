#lang htdp/isl+

(define (insertion-sort alon)
  (local ((define (sort alon)
            (cond
              [(empty? alon) '()]
              [else
               (add (first alon) (sort (rest alon)))]))
          (define (add an alon)
            (cond
              [(empty? alon) (list an)]
              [else
               (cond
                 [(> an (first alon)) (cons an alon)]
                 [else (cons (first alon)
                             (add an (rest alon)))])])))
    (sort alon)))

(define (sort-2 alon)
  (local ((define (sort alon)
            (cond
              [(empty? alon) '()]
              [else
               (add (first alon) (sort (rest alon)))]))
          (define (add an alon)
            (cond
              [(empty? alon) (list an)]
              [else
               (cond
                 [(> an (first alon)) (cons an alon)]
                 [else (cons (first alon)
                             (add an (rest alon)))])])))
    (sort alon)))
