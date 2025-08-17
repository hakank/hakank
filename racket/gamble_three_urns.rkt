#| 

  Three urns in Racket/Gamble 

  From Andreas Stuhlmüller 
  "Modeling Cognition with Probabilistic Programs: Representations and Algorithms"
  page 30f
  """
  We are presented with three opaque urns, each of which contains some unknown number of
  red and black balls. We do not know the proportion of red balls in each urn, and
  we don’t know how similar the proportions of red balls are between urns, but we
  have reason to suspect that the urns could be similar, as they all were filled at the
  same factory. We are asked to predict the proportion of red balls in the third urn
  (1) before making any observations, 
  (2) after observing 15 balls drawn from the first urn, 14 of which are red, and 
  (3) after observing in addition 15 balls drawn from the second urn, only one 
      of which is red.
  """

  Note: This model was written before I read the further in the text.

  I don't this that this is a good model since our view of the model 
  changes after the observations. So I don't publish it!

  Assumptions:
  - We replace the balls after each observation.
  - The three urns share a common probability of the proportion of red balls
    (p_red).
  - We assume that the number of balls in each urn is distributed 
    according to poisson 15.

  One issue is how much we should assume before making any observations. 
  Here I assume that that
  - there are at least 15 balls in urn 1 and at least 14 red balls in urn 1
  - there are at least 15 balls in urn 2 and at least 1 red ball in urn 2
  - there is at least one ball in urn 3

  * 1) Number of red in urn 3 before any observations

  variable : p_red
  mean: 0.5001716095280182

  variable : num_balls1
  mean: 17.874830000033775

  variable : num_balls2
  mean: 17.875400000033753

  variable : num_balls3
  mean: 15.003750000029484

  variable : urn1_red
  mean: 8.935780000016752

  variable : urn2_red
  mean: 8.944700000016768

  variable : urn3_red
  mean: 7.509810000014123

  variable : p_urn1_red
  mean: 0.49982214972160705

  variable : p_urn2_red
  mean: 0.5003815872117356

  variable : p_urn3_red
  mean: 0.5003500402601098


  * 2) After observing the number of red balls in urn 1 as 14
       what is the proportion of red in urn 3 (p_urn3_red)?

  variable : p_red
  mean: 0.7772714424294229

  variable : num_balls1
  mean: 17.568240000032418

  variable : num_balls2
  mean: 17.87487000003375

  variable : num_balls3
  mean: 15.002180000029476

  variable : urn1_red
  mean: 14.000000000054802

  variable : urn2_red
  mean: 13.881410000027765

  variable : urn3_red
  mean: 11.667100000022886

  variable : p_urn1_red
  mean: 0.8106074231396131

  variable : p_urn2_red
  mean: 0.7765666389200252

  variable : p_urn3_red
  mean: 0.7777408898957595


  * 3) After also observing the number of red balls in urn 2 as 1 in 15 drawn balls,
       what is the proportion of red in urn 3 (p_urn3_red)?
       Note: importance-sampler is too slow for this, so mh-sampler is used instead.

  variable : p_red
  mean: 0.3897895826402577

  variable : num_balls1
  mean: 23.11946000000169

  variable : num_balls2
  mean: 16.243190000001675

  variable : num_balls3
  mean: 15.096860000001117

  variable : urn1_red
  mean: 14.000000000001076

  variable : urn2_red
  mean: 1.0000000000000768

  variable : urn3_red
  mean: 5.883020000000436

  variable : p_urn1_red
  mean: 0.6176029958339799

  variable : p_urn2_red
  mean: 0.06203525727537025

  variable : p_urn3_red
  mean: 0.3895398476849319


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model question)
  (show "question" question)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   ; The common - but unknown - proportion of red in the urns.
   (define p_red (beta 1 1))

   ; Number of balls in each urn
   (define num_balls1 (poisson 15))
   (observe/fail (>= num_balls1 15))
    
   (define num_balls2 (poisson 15))
   (observe/fail (>= num_balls2 15))
    
   (define num_balls3 (poisson 15))
   (observe/fail (> num_balls3 0))      
   (observe/fail (>= num_balls3 1))
    
   ; number of red balls (== true)
   (define urn1_red (binomial num_balls1 p_red))
   (define urn2_red (binomial num_balls2 p_red))
   (define urn3_red (binomial num_balls3 p_red))   

   ; The proportion of red in each urn
   (define p_urn1_red (/ urn1_red num_balls1))
   (define p_urn2_red (/ urn2_red num_balls2))
   (define p_urn3_red (/ urn3_red num_balls3))
   

   ; 2) After observing the number of red balls in urn 1 as 14
   ;    what is the proportion of red in urn 3 (p_urn3_red)?
   (when (or (= question 2) (= question 3))
     (observe-sample (dist-unit urn1_red) 14) 
     )

   ; 3) After also observing the number of red balls in urn 2 as 15,
   ;    what is the proportion of red in urn 3 (p_urn3_red)?
   (when (= question 3)
     (observe-sample (dist-unit urn2_red) 1)
     )

   (list p_red
         num_balls1
         num_balls2
         num_balls3
         
         urn1_red
         urn2_red
         urn3_red
         
         p_urn1_red
         p_urn2_red
         p_urn3_red        
        )
   
   )
)

(for ([question '(1 2 3)])
  (show-marginals (model question)
                  (list  "p_red"
                         "num_balls1"
                         "num_balls2"
                         "num_balls3"
                         "urn1_red"
                         "urn2_red"
                         "urn3_red"
                         "p_urn1_red"
                         "p_urn2_red"
                         "p_urn3_red"                       
                         
                     )
                  #:num-samples 10000
                  #:truncate-output 5
                  #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:hpd-interval (list 0.84)
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ; #:burn 0
                  ; #:thin 10
                  )
  )

