#| 

  Streaks in chess in Racket/Gamble 

  From https://math.stackexchange.com/questions/417762/probability-of-20-consecutive-success-in-100-runs
  """
  Probability of 20 consecutive success in 100 runs.

  Suppose a chess player have a win rate equal 90%, what is the chance to have 
  20 consecutive wins (successes) playing 100 games? Consider that lose/draw = fail.

  I've studied basic statistics in college and it seems like a binomial 
  distribution problem (right?), but honestly I can't figure out a way to solve 
  this problem considering "consecutive" successes.

  Is there a statistical distribution for this kinda problem?

  Thanks very much! I really appreciate any thoughts!
  """
 
  From a comment:
  """
  Feller has this all worked out on p. 325 of An Introduction to Probability Theory 
  and Its Applications, 3rd Edition, equation 7.11:

  ...

  So the probability that the chess player will have at least one run of 20 
  successes is 0.7753, approximately.
  """

  The model gets about the same result (average value of p = 0.7757400000000001)
  Here we also check how many times the run is >= 20 (average: 1).
  Using importance-sampler and 100000 samples.

  variable : max-run
  22: 0.05255000000000002
  21: 0.05129000000000001
  23: 0.05118000000000001
  20: 0.04975000000000001
  24: 0.049130000000000014
  ...
  95: 2.0000000000000005e-5
  87: 1.0000000000000003e-5
  92: 1.0000000000000003e-5
  96: 1.0000000000000003e-5
  98: 1.0000000000000003e-5
  mean: 26.90456000000001
  HPD interval (0.94): 11..44
  HPD interval (0.99): 9..58
  HPD interval (0.999): 9..76
  HPD interval (0.9999): 8..88
  HPD interval (0.99999): 8..100
  Histogram:
  8:    2 # (2e-5  / 0    )
  9:   26 # (0.000 / 2e-5 )
 10:  110 ## (0.001 / 0.000)
 11:  295 ##### (0.002 / 0.001)
 12:  622 ########## (0.006 / 0.004)
 13: 1139 ################## (0.011 / 0.010)
 14: 1738 ########################### (0.017 / 0.021)
 15: 2480 ####################################### (0.024 / 0.039)
 16: 3188 ################################################## (0.031 / 0.064)
 17: 3845 ############################################################ (0.038 / 0.096)
 18: 4437 ##################################################################### (0.044 / 0.134)
 19: 4730 ########################################################################## (0.047 / 0.178)
 20: 5151 ################################################################################ (0.051 / 0.226)
 21: 5113 ################################################################################ (0.051 / 0.277)
 22: 5071 ############################################################################### (0.050 / 0.328)
 23: 5012 ############################################################################## (0.050 / 0.379)
 24: 4859 ############################################################################ (0.048 / 0.429)
 25: 4613 ######################################################################## (0.046 / 0.478)
 26: 4414 ##################################################################### (0.044 / 0.524)
 27: 4074 ################################################################ (0.040 / 0.568)
 28: 3714 ########################################################## (0.037 / 0.609)
 29: 3534 ####################################################### (0.035 / 0.646)
 30: 3277 ################################################### (0.032 / 0.681)
 31: 2907 ############################################## (0.029 / 0.714)
 32: 2710 ########################################### (0.027 / 0.743)
 33: 2387 ###################################### (0.023 / 0.770)
 34: 2114 ################################# (0.021 / 0.794)
 35: 2000 ################################ (0.02  / 0.815)
 36: 1824 ############################# (0.018 / 0.835)
 37: 1518 ######################## (0.015 / 0.853)
 38: 1437 ####################### (0.014 / 0.869)
 39: 1262 #################### (0.012 / 0.883)
 40: 1212 ################### (0.012 / 0.896)
 41:  983 ################ (0.009 / 0.908)
 42:  935 ############### (0.009 / 0.917)
 43:  830 ############# (0.008 / 0.927)
 44:  703 ########### (0.007 / 0.935)
 45:  672 ########### (0.006 / 0.942)
 46:  551 ######### (0.005 / 0.949)
 47:  516 ######### (0.005 / 0.954)
 48:  460 ######## (0.004 / 0.960)
 49:  379 ###### (0.003 / 0.964)
 50:  395 ####### (0.003 / 0.968)
 51:  335 ###### (0.003 / 0.972)
 52:  291 ##### (0.002 / 0.975)
 53:  227 #### (0.002 / 0.978)
 54:  240 #### (0.002 / 0.980)
 55:  205 #### (0.002 / 0.983)
 56:  165 ### (0.001 / 0.985)
 57:  144 ### (0.001 / 0.987)
 58:  161 ### (0.001 / 0.988)
 59:  115 ## (0.001 / 0.990)
 60:  111 ## (0.001 / 0.991)
 61:   90 ## (0.000 / 0.992)
 62:   81 ## (0.000 / 0.993)
 63:   77 ## (0.000 / 0.994)
 64:   69 ## (0.000 / 0.994)
 65:   56 # (0.000 / 0.995)
 66:   43 # (0.000 / 0.996)
 67:   49 # (0.000 / 0.996)
 68:   34 # (0.000 / 0.996)
 69:   27 # (0.000 / 0.997)
 70:   36 # (0.000 / 0.997)
 71:   24 # (0.000 / 0.997)
 72:   21 # (0.000 / 0.998)
 73:   20 # (0.000 / 0.998)
 74:   17 # (0.000 / 0.998)
 75:   13 # (0.000 / 0.998)
 76:   14 # (0.000 / 0.998)
 77:   10 # (0.000 / 0.999)
 78:   10 # (0.000 / 0.999)
 79:   12 # (0.000 / 0.999)
 80:   10 # (0.000 / 0.999)
 81:    6 # (6e-5  / 0.999)
 82:   11 # (0.000 / 0.999)
 83:    6 # (6e-5  / 0.999)
 84:   11 # (0.000 / 0.999)
 85:    2 # (2e-5  / 0.999)
 86:    4 # (4e-5  / 0.999)
 87:    1 # (1e-5  / 0.999)
 88:    4 # (4e-5  / 0.999)
 90:    2 # (2e-5  / 0.999)
 91:    2 # (2e-5  / 0.999)
 92:    1 # (1e-5  / 0.999)
 93:    1 # (1e-5  / 0.999)
100:    3 # (3e-5  / 0.999)

  variable : p
  #t: 0.7757899999999998
  #f: 0.22421000000000008
  mean: 0.7757899999999998
  Histogram:
  #f: 22612 ######################## (0.226 / 0    )
  #t: 77388 ################################################################################ (0.773 / 0.226)
  
  variable : num-runs>=check
  1: 0.48726000000000025
  2: 0.2581600000000001
  0: 0.22421000000000008
  3: 0.030070000000000006
  4: 0.00030000000000000014
  mean: 1.0949900000000004
  HPD interval (0.94): 0..2
  HPD interval (0.99): 0..3
  HPD interval (0.999): 0..3
  HPD interval (0.9999): 0..4
  HPD interval (0.99999): 0..4
  Histogram:
  0: 22612 ###################################### (0.226 / 0    )
  1: 48824 ################################################################################ (0.488 / 0.226)
  2: 25527 ########################################## (0.255 / 0.714)
  3:  3011 ##### (0.030 / 0.969)
  4:    26 # (0.000 / 0.999)


  This also shows there is a (very small) chance that the player wins all 100 matches.
  
  Cf gamble_streaks.rkt and gamble_runs.rkt

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 100)
   (define prob 9/10)
   (define check 20) ; the number of win runs we want to check

   ; (define matches (for/list ([i n]) (bernoulli prob)))
   ; (define runs (get-runs matches))
   ; We only care about the winning runs
   ; (define win-runs (filter (lambda (run) (and (= (first run) 1))) runs))

   (define runs (generate-01-runs n prob))
   ; We only care about the winning runs
   (define win-runs (filter-runs runs 1))
   
   (define win-runs-lens (map length win-runs))
   (define max-run (max-list win-runs-lens))

   (define p (>= max-run check))
   (define num-runs>=check (for/sum ([run win-runs-lens]) (b2i (>= run check))))

   (list max-run
         p
         num-runs>=check
         )

   )
)

(show-marginals (model)
                (list  "max-run"
                       "p"
                       "num-runs>=check"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.94 0.99 0.999 0.9999 0.99999)
                    #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )

(displayln "\nTheoretical:")
(let* ([n 100]
       [p 90/100]
       [r 20]
       [t (probability-of-run-size n p r)])
  (show2 "t" t (* 1.0 t)))
  
