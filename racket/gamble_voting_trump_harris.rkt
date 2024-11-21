#| 

  Probability of winning an election - Trump vs Harris in Racket/Gamble 

  What is the probability of Trump or Harris (or other) winning given forecasts,
  i.e. that they get more votes than the two other?

  From Nate Silver
  (as of writing 2024-11-03 @09:34, CEST)
  https://www.natesilver.net/p/nate-silver-2024-president-election-polls-model

  Probability of national winning;
  - Trump: 0.476
  - Harris: 0.485
   

  Here are some runs with different number of votes.

  * trump-prob:0.476 harris-prob:0.485 num-votes:10
  variable : trump votes
  5: 0.2432717836159391
  4: 0.22316949337316544
  6: 0.18415612118509384
  3: 0.14038513148564066
  7: 0.09559249038615558
  2: 0.05795310574984956
  8: 0.03256347239299383
  1: 0.014177136980822208
  9: 0.006573457531410122
  0: 0.0015606764239392522
  10: 0.00059713087499069
  mean: 4.760000000000002

  variable : harris votes
  5: 0.24498831969115878
  4: 0.21678519697757181
  6: 0.19226429619775406
  3: 0.13153976606002887
  7: 0.10346539101904101
  2: 0.05237859241307849
  8: 0.036539355323471995
  1: 0.012359667854632498
  9: 0.007646855375138692
  0: 0.0013124183392032441
  10: 0.0007201407489208285
  mean: 4.850000000000001

  variable : other votes
  0: 0.6717905288190823
  1: 0.27263091179962773
  2: 0.04978847556798616
  3: 0.005388138875203501
  4: 0.0003826643894200201
  5: 1.863547702898744e-5
  6: 6.302320535297528e-7
  7: 1.4615162828993861e-8
  8: 2.2242118249119173e-10
  9: 2.0058795507356868e-12
  10: 8.140406085191632e-15
  mean: 0.39000000000000024

  variable : trump wins
  mean: 0.3838158687617167

  variable : harris wins
  mean: 0.4051461522401703

  variable : other wins
  mean: 8.798337287047944e-6

  variable : no-win
  mean: 0.21102918066082618


  * trump-prob:0.476 harris-prob:0.485 num-votes:20
  variable : trump votes
  mean: 9.52

  variable : harris votes
  mean: 9.699999999999998

  variable : other votes
  mean: 0.7800000000000006

  variable : trump wins
  mean: 0.4296248402795307

  variable : harris wins
  mean: 0.46216068059593873

  variable : other wins
  mean: 9.006139042244392e-8

  variable : no-win
  mean: 0.10821438906314013


  * trump-prob:0.476 harris-prob:0.485 num-votes:100
  variable : trump votes
  mean: 47.600000000000065

  variable : harris votes
  mean: 48.50000000000007

  variable : other votes
  mean: 3.900000000000006

  variable : trump wins
  mean: 0.4432077704761368

  variable : harris wins
  mean: 0.5163552718926219

  variable : other wins
  mean: 8.28435991822703e-25

  variable : no-win
  mean: 0.04043695763124203


  For num-votes >= 10_000 we use importance-sampler (100000 samples)

  * trump-prob:0.476 harris-prob:0.485 num-votes:1000
  variable : trump votes
  mean: 476.00000000000495

  variable : harris votes
  mean: 485.0000000000052

  variable : other votes
  mean: 39.00000000000043

  variable : trump wins
  mean: 0.3796154095853963

  variable : harris wins
  mean: 0.6080495989930508

  variable : other wins
  mean: 5.032377741476163e-210

  variable : no-win
  mean: 0.012334991421558672


  * trump-prob:0.476 harris-prob:0.485 num-votes:10000
  variable : trump votes
  mean: 4759.884940000492

  variable : harris votes
  mean: 4850.175340000505

  variable : other votes
  mean: 389.93972000004067

  variable : trump wins
  mean: 0.17680000000002394

  variable : harris wins
  mean: 0.8203600000000822

  variable : other wins
  mean: 0 (0.0)

  variable : no-win
  mean: 0.002840000000000296

  * trump-prob:0.476 harris-prob:0.485 num-votes:100000
  variable : trump votes
  mean: 47599.809810039114

  variable : harris votes
  mean: 48499.93005003991

  variable : other votes
  mean: 3900.2601400032117
  
  variable : trump wins
  mean: 0.0019600000000016176

  variable : harris wins
  mean: 0.9980299999995156

  variable : other wins
  mean: 0 (0.0)

  variable : no-win
  mean: 1.0000000000008212e-5

  * trump-prob:0.476 harris-prob:0.485 num-votes:1000000
  variable : trump votes
  mean: 475998.19276081974

  variable : harris votes
  mean: 485001.83436083555

  variable : other votes
  mean: 38999.97288006713

  variable : trump wins
  mean: 0 (0.0)

  variable : harris wins
  mean: 1.0000000000037073

  variable : other wins
  mean: 0 (0.0)

  variable : no-win
  mean: 0 (0.0)



  Cf gamble_voting_probabilities.rkt

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


#|


|#
(define (model trump-prob harris-prob num-votes)
  (enumerate
   ; importance-sampler

   (define other-prob (- 1 trump-prob harris-prob))

   (define m (multinomial_dist num-votes (list trump-prob harris-prob other-prob)))
   (define trump (first m))
   (define harris (second m))
   (define other (third m))
        
   (define trump-wins (> trump harris other))
   (define harris-wins (> harris trump other))
   (define other-wins (> other trump harris))

   (define no-win (not (or trump-wins harris-wins other-wins)))
   
   (list trump
         harris
         other
         trump-wins
         harris-wins
         other-wins
         no-win
         )
   
   )
)

(let ([trump-prob 0.476]
      [harris-prob 0.485]
      )
  (for ([num-votes '(10 20 100 1000)])
    (newline)
    (displayln (format "* trump-prob:~a harris-prob:~a num-votes:~a" trump-prob harris-prob num-votes))
    (show-marginals (model trump-prob harris-prob num-votes)
                    (list "trump votes"
                          "harris votes"
                          "other votes"
                          "trump wins"
                          "harris wins"
                          "other wins"
                          "no-win"
                          )
                    #:num-samples 100000
                    ; #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )
    (flush-output)
    )

  )


