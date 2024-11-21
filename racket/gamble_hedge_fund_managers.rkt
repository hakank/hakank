#| 

  Hedge fund managers in Racket/Gamble 

  From Mathematica OrderStatistics 
  """
  Find the probability that the most successful hedge-fund manager among 
  25 non-skilled managers outperforms the market nine out of 10 years, assuming 
  their performances are independent from each other, and from year to year:

  managerSuccessDist = BinomialDistribution[10, 1/2];
  NProbability[goodYears >= 9, goodYears e OrderDistribution[{managerSuccessDist, 25}, 25]]
  -> 0.236626

  Compare to the probability that one a priori chosen manager performs this well:
  NProbability[goodYears >= 9, goodYears e managerSuccessDist]
  -> 
  0.0107422
  """


  Model 1
  var : bestm
  8: 0.5171500000000002
  7: 0.23730000000000004
  9: 0.21200000000000005
  10: 0.024120000000000006
  6: 0.009430000000000001
  mean: 8.004080000000002

  var : p-best
  #f: 0.7638800000000001
  #t: 0.23612000000000008
  mean: 0.23612000000000008

  var : p-first
  #f: 0.9895600000000001
  #t: 0.010440000000000001
  mean: 0.010440000000000001


  * A related problem: 
    How many of the hedge fund  managers outperforms the market nine out of ten years,

  Model 2
  var : p25
  0: 0.7662900000000001
  1: 0.20497
  2: 0.026540000000000005
  3: 0.0021200000000000004
  4: 7.000000000000001e-5
  5: 1.0000000000000003e-5
  mean: 0.26474

  var : p1
  0: 0.98991
  1: 0.01009
  mean: 0.01009

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 25) ; Number of hedge fund managers

   (define managers (for/list ([i n]) (binomial 10 1/2)))
   ; The best mananger
   (define bestm (apply max managers))
   (define p-best (>= bestm 9))

   ; A random manager (e.g. the first)
   (define firstm (first managers))
   (define p-first (>= firstm 9))
  
   (list bestm
         p-best
         ; firstm
         p-first
         )
   

   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list  "bestm"
                       "p-best"
                       ; "firstm"
                       "p-first"
                     )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )




(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 25) ; Number of hedge fund managers

   (define managers (for/list ([i n]) (boolean->integer (>= (binomial 10 1/2) 9))))
   (define p25 (sum managers))
   (define p1 (list-ref managers 0))
  
   (list p25
         p1
         )
   

   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "p25"
                       "p1"
                     )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


