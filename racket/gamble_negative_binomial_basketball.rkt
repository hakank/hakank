#| 

  Negative Binomial basketball in Racket.Gamble 

  From Mathematica NegativeBinomialDistribution
  """ 
  A basketball player shoots free throws until he hits 4 of them. His probability 
  of scoring in any one of them is 0.7. 

  Simulate the process:
  ...

  Find the number of shots the player is expected to shoot:
  > Expectation[x + 4, x => NegativeBinomialDistribution[4, 0.7]]
  5.71429

  Find the probability that the player requires 4 shots:
  > NProbability[x + 4 == 4, x => NegativeBinomialDistribution[4, 0.7]]
  0.2401
  """

  Enumerate :#limit 1e-05
 
var : x
1: 0.2881228795191715
0: 0.24010239959930954
2: 0.21609215963937856
3: 0.12965529578362717
4: 0.06806903028640433
5: 0.032673134537474065
6: 0.014702910541863293
7: 0.006301247375084264
8: 0.0025992645422222562
9: 0.0010397058168889045
10: 0.0004054852685866739
11: 0.00015482164800582115
12: 5.805811800218288e-5
13: 2.14368435700367e-5
14: 2.1704804114662285e-6
mean: 1.7141561974704735

var : y
5: 0.2881228795191715
4: 0.24010239959930954
6: 0.21609215963937856
7: 0.12965529578362717
8: 0.06806903028640433
9: 0.032673134537474065
10: 0.014702910541863293
11: 0.006301247375084264
12: 0.0025992645422222562
13: 0.0010397058168889045
14: 0.0004054852685866739
15: 0.00015482164800582115
16: 5.805811800218288e-5
17: 2.14368435700367e-5
18: 2.1704804114662285e-6
mean: 5.714156197470473

var : p
#f: 0.7598976004006908
#t: 0.24010239959930954
mean: 0.24010239959930954

Calculations via negative_binomial family

Simulate the process:
'(5 0 2 2 2 1 2 3 0 0 3 2 2 1 0 2 3 1 1 1)

Find the number of shots the player is expected to shoot:
1.7221

Find the probability that the player requires 4 shots:
0.24009999999999995

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-10
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define b-n 4)
   (define b-p 0.7)
   
   (define x (negative_binomial b-n b-p))
   ; Number of actual throws needed
   (define y (+ x 4))

   ; What is the probability that it requires 4 shots
   (define p (= y 4 ))

   (list x
         y
         p
         )
   )
)

(show-marginals (model)
                (list  "x"
                       "y"
                       "p"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


(displayln "\nCalculations via negative_binomial family")

(displayln "\nSimulate the process:")
(repeat (lambda () (negative_binomial 4 0.7)) 20)

(displayln "\nFind the number of shots the player is expected to shoot:")
(* 1.0 (avg (repeat (lambda () (negative_binomial 4 0.7)) 100000)))

(displayln "\nFind the probability that the player requires 4 shots:")
(negative_binomial_pdf 4 0.7 0)

