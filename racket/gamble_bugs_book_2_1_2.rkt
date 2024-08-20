#| 

  BUGS book, 2.1.2 in Racket Gamble.

  Page 17

4: 0.27343750000000006
3: 0.21874999999999997
5: 0.21874999999999997
2: 0.10937499999999996
6: 0.10937499999999996
1: 0.03124999999999999
7: 0.03124999999999999
0: 0.003906250000000003
8: 0.003906250000000003
mean: 4.0

var : p2
#f: 0.85546875
#t: 0.14453124999999994
mean: 0.14453124999999994

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")


(define (bugs-book-2-1-2)
  (enumerate

   (define y (binomial 8 1/2))
   (define p2 (<= y 2))

    (list y p2)
   )
  )

(show-marginals (bugs-book-2-1-2) (list "y" "p2"))
