#| 

  Galileo's dice in Racket/Gamble 

  From Mathematica DiscreteUniformDistribution
  """
  Solve Galileo's problem to determine the odds of getting 9 points versus 10 
  points obtained in throws of three dice:

  Although the number of integer partitions of 10 and 9 into a sum of three numbers 
  1-6 are the same:
  pointsD =  TransformedDistribution[ d1 + d2 + d3, {d1, d2, d3} e
                          ProductDistribution[{DiscreteUniformDistribution[{1, 6}], 3}]];
   ...

  Odds of getting 10 points are higher:
  odds = PDF[pointsD, 9]/ PDF[pointsD, 10]
  -> 25/27
  """

  variable : s
  10: 1/8 (0.125)
  11: 1/8 (0.125)
  9: 25/216 (0.11574074074074074)
  12: 25/216 (0.11574074074074074)
  8: 7/72 (0.09722222222222222)
  13: 7/72 (0.09722222222222222)
  7: 5/72 (0.06944444444444445)
  14: 5/72 (0.06944444444444445)
  6: 5/108 (0.046296296296296294)
  15: 5/108 (0.046296296296296294)
  16: 1/36 (0.027777777777777776)
  5: 1/36 (0.027777777777777776)
  17: 1/72 (0.013888888888888888)
  4: 1/72 (0.013888888888888888)
  18: 1/216 (0.004629629629629629)
  3: 1/216 (0.004629629629629629)
  mean: 21/2 (10.5)

  variable : p0
  #f: 191/216 (0.8842592592592593)
  #t: 25/216 (0.11574074074074074)
  mean: 25/216 (0.11574074074074074)

  variable : p10
  #f: 7/8 (0.875)
  #t: 1/8 (0.125)
  mean: 1/8 (0.125)

  p9: 25/216 (0.11574074074074074) p10: 1/8 (0.125) p9/p10: 25/27 (0.9259259259259259)

  Here are the integer partitions (sorted):
   (part10 ((1 3 6) (1 4 5) (2 2 6) (2 3 5) (2 4 4) (3 3 4)) len 6)
   (part9 ((1 2 6) (1 3 5) (1 4 4) (2 2 5) (2 3 4) (3 3 3)) len 6)

  They are both of length 6 as mentioned in the problem statement.

  However, if we don't care about sorted lists, then we get another result:
  (part10 ((1 3 6) (1 4 5) (1 5 4) (1 6 3) (2 2 6) (2 3 5) (2 4 4) (2 5 3) (2 6 2) (3 1 6) (3 2 5) (3 3 4) (3 4 3) (3 5 2) (3 6 1) (4 1 5) (4 2 4) (4 3 3) (4 4 2) (4 5 1) (5 1 4) (5 2 3) (5 3 2) (5 4 1) (6 1 3) (6 2 2) (6 3 1)) len 27)
  (part9 ((1 2 6) (1 3 5) (1 4 4) (1 5 3) (1 6 2) (2 1 6) (2 2 5) (2 3 4) (2 4 3) (2 5 2) (2 6 1) (3 1 5) (3 2 4) (3 3 3) (3 4 2) (3 5 1) (4 1 4) (4 2 3) (4 3 2) (4 4 1) (5 1 3) (5 2 2) (5 3 1) (6 1 2) (6 2 1)) len 25)

  For 10 there are 27 possible ways, but for 9 there are only 25. Thus we get
  the odds 25/27.


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (enumerate

   (define n 3)
   (define ds (for/list ([i n]) (discrete_uniform_dist 1 6)))

   (define s (sum ds))
   (define p9 (= s 9))
   (define p10 (= s 10))
   (list s
         p9
         p10)
   
   )
)

(show-marginals (model)
                (list  "s"
                       "p0"
                       "p10"
                     )
                    )

; A neater way
(define p9 (get-prob-value (get-probs (model) #:ix 1) #t))
(define p10 (get-prob-value (get-probs (model) #:ix 2) #t))
(displayln (format "p9: ~a (~a) p10: ~a (~a) p9/p10: ~a (~a)" p9 (* 1.0 p9) p10 (* 1.0 p10) (/ p9 p10) (* 1.0 (/ p9 p10)) ))
(newline)


(displayln "Partitions (sorted)")
(define part10s (integer-partition 10 3 (range 1 7) #t))
(define part9s (integer-partition 9 3 (range 1 7) #t))
(show2 "part10" part10s "len" (length part10s))
(show2 "part9" part9s "len" (length part9s))

(displayln "Partitions (unsorted)")
(define part10 (integer-partition 10 3 (range 1 7) #f))
(define part9 (integer-partition 9 3 (range 1 7) #f))
(show2 "part10" part10 "len" (length part10))
(show2 "part9" part9 "len" (length part9))
(newline)
  
