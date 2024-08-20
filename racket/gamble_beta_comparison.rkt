#| 

  Beta comparison in Racket Gamble.

  From infer.net src/tests/Test/BlogTests.cs
  """
  a = Beta(11,500)[mean=0.02153], b = Beta(16,695)[mean=0.0225]
  aGreaterThanConstant = Bernoulli(0.9849)
  P(A > B) =  Bernoulli(0.4467)
  """

  (This is yet another A/B test.)


  This is a port of my WebPPL model beta_comparison.wppl

  Output:
var : aRate
0.02153918195307803: 0.05401444511130442
0.018792363066265028: 0.051956408452580906
0.02387505622071582: 0.049690142258395846
0.019741125847142237: 0.04827997472391626
0.018808812454752834: 0.04785821258966435
...
0.05913624895972723: 8.514932666281792e-146
0.04319759688122142: 5.855759020139054e-162
0.24449534903120826: 4.5392319438423066e-183
0.07110235885686753: 7.011371397002821e-193
0.20982492152112572: 8.906076756349039e-225
mean: 0.021406359584807258

var : bRate
0.02334884478160272: 0.05401444511130442
0.01899960659263998: 0.051956408452580906
0.021485507637408894: 0.049690142258395846
0.02505826536451433: 0.04827997472391626
0.024968863402222673: 0.04785821258966435
...
0.42729204157552253: 8.514932666281792e-146
0.46252644016555783: 5.855759020139054e-162
0.420798061333166: 4.5392319438423066e-183
0.5098545621570651: 7.011371397002821e-193
0.5135026492403301: 8.906076756349039e-225
mean: 0.022188959763805017

var : aRate>bRate
#f: 0.5662089664356257
#t: 0.4337910335643743
mean: 0.4337910335643743

var : aRate>0.0
#t: 0.9773609055255981
#f: 0.022639094474402172
mean: 0.9773609055255981




  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

(define (beta-comparison)
  (importance-sampler
   ; mh-sampler

    (define aRate (beta 1 10))
    (define bRate (beta 1 10))
    
    (define aTrialCount 500)
    (define bTrialCount 700)
    
    (define aSuccessCount (binomial-dist aTrialCount aRate))
    (define bSuccessCount (binomial-dist bTrialCount bRate))
    
    (observe-sample aSuccessCount 10)
    (observe-sample bSuccessCount 15)

    ; return values
    (list aRate bRate (> aRate bRate) (> aRate 0.01))

   )

  )

(show-marginals (beta-comparison)
                (list "aRate" "bRate" "aRate>bRate" "aRate>0.0"1)
                #:num-samples 10000
                #:truncate-output 5)

      
