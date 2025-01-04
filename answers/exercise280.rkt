#lang htdp/isl+

(check-expect ((lambda (x y) (+ x (* x y)))
               1 2)
              3)

(check-expect ((lambda (x y)
                 (+ x
                    (local ((define z (* y y)))
                      (+ (* 3 z) (/ 1 x)))))
               1 2)
              14)

(check-expect ((lambda (x y)
                 (+ x
                    ((lambda (z)
                       (+ (* 3 z) (/ 1 z)))
                     (* y y))))
               1 2)
              13.25)
