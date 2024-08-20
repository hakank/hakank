#| 

  A/B testing in Racket Gamble.

  From http://rpubs.com/rasmusab/exercise_2_bayesian_ab_testing
  """
  Exercise 2: Bayesian A/B testing for Swedish Fish Incorporated with Stan
  Rasmus Bååth

  Swedish Fish Incorporated is the largest Swedish company delivering fish by mail order, 
  but you probably already knew that. The marketing department have done a pilot study and 
  tried two different marketing methods:
  
  A: Sending a mail with a colorful brochure that invites people to sign up for a one year 
     salmon subscription.
  
  B: Sending a colorful brochure that invites people to sign up for a one year 
     salmon subscription nd that includes a free salmon.
  
  The marketing department sent out 16 mails of type A and 16 mails of type B. 
  Six Danes that received a mail of type A signed up for one year of salmon, and 
  ten Danes that received a mail of type B signed up!
  
  The marketing department now wants to know, which method should we use, A or B?
  
  At the bottom of this document you’ll find a solution. But try yourself first!
  Question I: Build a Bayesian model in Stan that answers the question: What is the 
  probability that method B is better than method A?
  """

  This is a port of my WebPPL model ab_testing.wppl

var : rateA
0.5669830066460337: 0.0009999999999999994
0.25583698978044417: 0.0009999999999999994
0.3779036359870705: 0.0009999999999999994
...
0.373592242297527: 0.0009999999999999994
0.43937911242033717: 0.0009999999999999994
0.32904803786473164: 0.0009999999999999994
mean: 0.393974918275089
Min: 0.0920991995736569 Mean: 0.386061808412862 Max: 0.7693634685193192 Variance: 0.01191659098885676 Stddev: 0.10916313933217915
Credible interval (0.84): 0.21269415766941033..0.521389455871891

var : rateB
0.6547043282022934: 0.0009999999999999994
0.4807883438197839: 0.0009999999999999994
0.6890234179601239: 0.0009999999999999994
...
0.5405375148243744: 0.0009999999999999994
0.5111249085315459: 0.0009999999999999994
0.6243520308903471: 0.0009999999999999994
mean: 0.6110504744617488
Min: 0.23197108629392135 Mean: 0.6085761971752733 Max: 0.9060646126655488 Variance: 0.013130535738190296 Stddev: 0.11458854976912089
Credible interval (0.84): 0.44265877387324004..0.7637779889315883

var : rate-diff
0.27903386252909984: 0.0009999999999999994
0.34133415087068064: 0.0009999999999999994
0.008314470930353202: 0.0009999999999999994
...
0.13154127783155667: 0.0009999999999999994
0.07218202622930092: 0.0009999999999999994
0.23513414196388366: 0.0009999999999999994
mean: 0.21707555618666
Min: -0.25333543580346063 Mean: 0.22251438876241292 Max: 0.6317134407340539 Variance: 0.024072521092515866 Stddev: 0.15515321811846464
Credible interval (0.84): 0.02182947717153727..0.4601985008272548

var : diff-pos
#t: 0.9130000000000007
#f: 0.08700000000000005
mean: 0.9130000000000007
Min: 0 Mean: 0.914 Max: 1 Variance: 0.078604 Stddev: 0.28036404905051576
Credible interval (0.84): 1..1


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (ab-testing)
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; faster but not as reliable (as importance/rejection-sampler)
   
   (define nA 16) ; Number of sent mail
   (define nB 16)
    
   (define obsSA 6) ; Number of signments (observed)
   (define obsSB 10)
    
   (define rateA (uniform 0 1)) ; priors
   (define rateB (uniform 0 1))
    
   (define sA (binomial nA rateA)); likelihood
   (define sB (binomial nB rateB))
    
   (define rate-diff (- rateB rateA))
   (define diff-pos  (> rateB rateA))
    
   (observe/fail(= sA obsSA))
   (observe/fail(= sB obsSB))
   
   (list rateA rateB rate-diff diff-pos)
   
   )
  )

(show-marginals (ab-testing) '("rateA" "rateB" "rate-diff" "diff-pos")
                #:truncate-output 3
                #:show-stats? #t
                #:credible-interval 0.84
                )
