#| 

  Size of group of people in Racket/Gamble 

  https://pedrozudo.github.io/docs/20_thesis/thesis_final_online_compressed.pdf
  page 86
  """
  Example 6.2. In this example we show how to model the size of a group of people
  as a Poisson distribution and answer queries about the size of the group using the
  comparison predicates > / 2 and =:= / 2 .
  1 n_people ~ poisson(6).
  2
  3 more_than_five:- n_people>5.
  4 exactly_five:- n_people=:=5.
  5
  6 query(more_than_five).
  7 query(exactly_five).
  """

  Using enumerate #:limit 1e-30
  variable : n_people
  5: 0.1606231410479801
  6: 0.1606231410479801
  7: 0.1376769780411258
  4: 0.13385261753998343
  8: 0.10325773353084433
  3: 0.08923507835998895
  9: 0.06883848902056292
  2: 0.04461753917999446
  10: 0.04130309341233773
  11: 0.022528960043093314
  1: 0.01487251305999815
  12: 0.011264480021546657
  13: 0.00519899077917538
  0: 0.0024787521766663585
  14: 0.0022281389053608763
  15: 0.0008912555621443513
  16: 0.00033422083580413145
  17: 0.0001179602949896935
  18: 3.932009832989779e-5
  19: 1.2416873156809845e-5
  20: 3.7250619470429537e-6
  21: 1.064303413440844e-6
  22: 2.9026456730204814e-7
  23: 7.572119147009949e-8
  24: 1.8930297867524865e-8
  25: 4.5432714882059805e-9
  26: 1.0484472665090704e-9
  27: 2.329882814464608e-10
  28: 4.992606030995576e-11
  29: 1.0329529719301217e-11
  30: 2.065905943860238e-12
  31: 3.998527633277881e-13
  32: 7.497239312396033e-14
  33: 1.3631344204356422e-14
  34: 2.40553133018055e-15
  mean: 5.999999999999985

  variable : more_than_five
  #t: 0.5543203586353884
  #f: 0.4456796413646114
  mean: 0.5543203586353884

  variable : exactly_five
  #f: 0.8393768589520197
  #t: 0.1606231410479801
  mean: 0.1606231410479801

  > 5: (- 1 (poisson_dist_cdf 6 5)): 0.5543203586353888
  = 5: (poisson_dist_pdf 6 5): 0.16062314104798003


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-30

   (define n_people (poisson 6))
   (define more_than_five (> n_people 5))
   (define exactly_five (= n_people 5))

   (list n_people
         more_than_five
         exactly_five
         )
   )
)

(show-marginals (model)
                (list  "n_people"
                       "more_than_five"
                       "exactly_five"))

(newline)
(displayln (format "> 5: (- 1 (poisson_dist_cdf 6 5)): ~a" (- 1 (poisson_dist_cdf 6 5))))
(displayln (format "= 5: (poisson_dist_pdf 6 5): ~a" (poisson_dist_pdf 6 5)))
