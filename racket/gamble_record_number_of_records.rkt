#| 

  Number of records for permutations and normal distribution in Racket/Gamble 

  How many records are there in a sequence of n elements?

  This calculates the probabilities of the number of records for n values.

  The theoretical values is quite good for permutations and continuous
  distributions. For discrete distributions, e.g. uniform discrete, binomial, etc,
  this does not apply. (I'm still trying to find out theories for these distributions.)


  For n=10 (permutation of 1..10) the probabilities of 1..10 records are exactly:

  Theoretical:
  (1/10 7129/25200 1303/4032 4523/22680 19/256 3013/172800 1/384 29/120960 1/80640 1/3628800)

  3: 1303/4032 (0.32316468253968256)
  2: 7129/25200 (0.2828968253968254)
  4: 4523/22680 (0.1994268077601411)
  1: 1/10 (0.1)
  5: 19/256 (0.07421875)
  6: 3013/172800 (0.017436342592592594)
  7: 1/384 (0.0026041666666666665)
  8: 29/120960 (0.00023974867724867725)
  9: 1/80640 (1.240079365079365e-5)
  10: 1/3628800 (2.755731922398589e-7)
  mean: 7381/2520 (2.9289682539682538)

  For the mean of the number of records, one estimate is:
  - log n + euler's gamma

  * For n=100, k=3 (importance-sampler, 10000 samples)
  Theoretical number of records: '(0.01 0.051773775176396204 0.12585177029987651 0.1929860262082136 0.2112044151077197 0.17671664238107343 0.11815074889853093 0.06510095668852567 0.030244506231894362 0.012057409327337752 0.004182927290251766 0.0012772207451861096 0.0003465055018178377 8.419146068937442e-5 ...)

  variable : num-records
  5: 0.21100000000000033
  4: 0.19250000000000042
  6: 0.17290000000000053
  3: 0.12720000000000048
  7: 0.11920000000000038
  8: 0.06550000000000021
  2: 0.05220000000000015
  9: 0.0320000000000001
  10: 0.011600000000000034
  1: 0.010100000000000027
  11: 0.004000000000000011
  12: 0.0013000000000000036
  13: 0.0005000000000000013
  mean: 5.1870000000000145
  HPD interval (0.84): 2..7
  HPD interval (0.99): 1..10
  HPD interval (0.99999): 1..16

  variable : harmonic_number n
  5.187377517639621: 0.9999999999999506
  mean: 5.187377517639364

  * Here is n=10 for normal 100 15, importance-sampler 1000000
    which does match the theoretical probabilities fairly good:

  variable : num-records
  3: 0.32301
  2: 0.282573
  4: 0.199619
  1: 0.100189
  5: 0.074336
  6: 0.017415
  7: 0.002595
  8: 0.000251
  9: 1.1e-5
  10: 1e-6
  mean: 2.9292930000000004
  HPD interval (0.84): 1..4
  HPD interval (0.99): 1..6
  HPD interval (0.99999): 1..9

  variable : harmonic_number n
  2.9289682539682538: 1.0
  mean: 2.9289682539682538

  variable : est: log n + Euler's gamma
  2.8798007578955787: 1.0
  mean: 2.8798007578955787


  * n=10 uniform 0 1 (importance-samler 1000000 samples)
  variable : num-records
  3: 0.32355599999999995
  2: 0.28243399999999996
  4: 0.19959399999999994
  1: 0.09955099999999997
  5: 0.07460199999999999
  6: 0.017443999999999998
  7: 0.0025489999999999996
  8: 0.00025599999999999993
  9: 1.3999999999999996e-5
  mean: 2.9311539999999994
  HPD interval (0.84): 1..4
  HPD interval (0.99): 1..6
  HPD interval (0.99999): 1..9

  variable : harmonic_number n
  2.9289682539682538: 0.9999999999999998
  mean: 2.9289682539682533

  variable : est: log n + Euler's gamma
  2.8798007578955787: 0.9999999999999998
  mean: 2.879800757895578


  * discrete distributions such as discrete uniform (random integer, uniform-draw) 
    has another probabilities.

    Here is an example of n=8 and (random-integer ), importance-sampler,
    which shows that the probabiltiies and the mean are different from
    the theoretical values

   variable : num-records
   2: 0.37350000000000005
   3: 0.30650000000000005
   1: 0.15930000000000002
   4: 0.12580000000000002
   5: 0.031700000000000006
   6: 0.0031000000000000003
   7: 0.00010000000000000002
   mean: 2.506800000000001
   HPD interval (0.84): 1..3
   HPD interval (0.99): 1..5
   HPD interval (0.99999): 1..7

   variable : harmonic_number n
   2.9289682539682538: 1.0000000000000002
   mean: 2.928968253968254

   For example, the mean of the number of records (~2.5) is smaller than the theoretical
   (~2.93).
   
  * for n=8 (random-integer 8), enumerate

  Model
  variable : num-records
  2: 3432285/8388608 (0.4091602563858032)
  3: 609903/2097152 (0.29082441329956055)
  1: 206091/1048576 (0.19654369354248047)
  4: 1511559/16777216 (0.09009593725204468)
  5: 6615/524288 (0.012617111206054688)
  6: 6237/8388608 (0.0007435083389282227)
  7: 63/4194304 (1.5020370483398438e-5)
  8: 1/16777216 (5.960464477539063e-8)
  mean: 2427845/1048576 (2.315373420715332)

  variable : harmonic_number n
  2.717857142857143: 1 (1.0)
  mean: 2.717857142857143

  variable : est: log n + Euler's gamma
  2.6566572065813685: 1 (1.0)
  mean: 2.6566572065813685



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(require (only-in math/base
                    gamma.0
                    ))




(flush-output)

(displayln "\nModel")
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 10)
   (define limit n)

   ;
   ; Continuous distributions and permutations give a neat match
   ; with the theoretical values.
   ; 
   
   ; Permutations
   ; (define x (draw-without-replacement n (range n))) ; For enumerate
   ; (define x (shuffle (range n))) ; For importance-sampler
   ; (define x (draw-without-replacement n (range (* 2 n))))

   ; (define x (for/list ([i n]) (normal 100 15)))
   ; (define x (for/list ([i n]) (uniform 0 1)))
   ; (define x (for/list ([i n]) (exponential 1)))  
   ; (define x (for/list ([i n]) (cauchy 1 1)))  
   ; (define x (for/list ([i n]) (laplace 1 1)))  
   ; (define x (for/list ([i n]) (extreme_value_dist1)))
   ; (define x (for/list ([i n]) (extreme_value_dist2 1 1)))
   ; (define x (for/list ([i n]) (beta 10 10)))
   ; (define x (for/list ([i n]) (gamma 1 1)))
   ; (define x (for/list ([i n]) (chi_dist 1)))
   ; (define x (for/list ([i n]) (chi_squared_dist 1)))
   ; (define x (for/list ([i n]) (erlang 1 1)))
   ; (define x (for/list ([i n]) (inverse_exponential 3)))
   ; (define x (for/list ([i n]) (pareto 10000 30)))
   
   ; Other distributions, especially discrete distributions,
   ; does not fit the theoretical, i.e. they does not have the
   ; mean of about harmonic number n
   (define x (for/list ([i n]) (random-integer n))) ; Mean 2.5095700000000005
   ; (define x (for/list ([i n]) (poisson 10))) ; Mean 2.6591299999999993
   ; (define x (for/list ([i n]) (binomial n 1/2))) ; Mean 2.389029999999999
   ; (define x (for/list ([i n]) (hypergeometric2 3 10 20 ))) ; Mean 2.0063
   ; (define x (for/list ([i n]) (beta_binomial 10 1 1 ))) ; Mean 2.5519700000000007
   ; (define x (for/list ([i n]) (polya 10 1 1 1 ))) ; Mean 2.5485200000000003

   ; Unsure:
   ; (define x (for/list ([i n]) (negative_binomial 4 0.2 )))
   
   ; Mean: 2.94913 but the probabilities are not according to the theoretical probabilities
   ; (define x (scan + 0 (for/list ([i n]) (uniform-draw '(-1 1))))) ; random walk 1d

   ; Not applicable
   ; (define x (for/list ([i n]) (multinomial_dist 3 (list 1/3 1/3 1/3) )))

   
   (define records (get-records x))
   (define records-ix (get-records-ix x))
   (define num-records (length records-ix))

   (define h_n (* 1.0 (harmonic_number n)))
   ; estimate log n + Euler's gamma
   (define est (+ (log n) gamma.0) )
   
   (list num-records
         h_n
         est
         ; records-ix
         ; records
         )
   )
)

(show-marginals (model)
                (list  "num-records"
                       "harmonic_number n"
                       "est: log n + Euler's gamma"
                       "records_ix"
                       "records"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84 0.99 0.99999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


