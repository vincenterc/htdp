#lang htdp/isl+

(require 2htdp/abstraction)

(check-expect
 (for/list ([i 2] [j '(a b)]) (list i j))
 (list (list 0 'a) (list 1 'b)))

(check-expect
 (for*/list ([i 2] [j '(a b)]) (list i j))
 (list (list 0 'a) (list 0 'b) (list 1 'a) (list 1 'b)))
