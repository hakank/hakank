#| 

  Jung's fish stories (coincidences) in Racket/Gamble 

  From http://www.dartmouth.edu/~chance/chance_news/recent_news/chance_news_7.10.html#Math%20and%20Media
  """
  As part of the symposium, Persi Diaconis gave a talk on coincidences. He discussed a 
  number of examples from his classic paper with Fred Mosteller, "Methods for Studying
  Coincidence", Journal of the American Statistical Association Dec. 1989, Vol. 84, 
  No. 408, 853-861. He started with an example from the work of the well-known 
  psychiatrist C. G. Jung. Jung felt that coincidences occurred far to often to be 
  attributed to chance. Diacoinis considered one of Jung's examples where Jung 
  observed, in one day, six incidences having to do with fish. To show that this could
  occur by chance, Diaconis suggested that we model the occurrence of fish stories by 
  a Poisson process over a period of six months with a rate of one fish story per day.
  Then we plot the times at which fish stories occur and move a 24-hour window over 
  this period to see if the window ever includes 6 or more events, i.e., fish stories.
  Diaconis remarks that finding the probability of this happening is not an easy 
  problem but can be done. The answer is that there is about a 22% chance that the 
  window will cover 6 or more events.
  """

  I think it's this talk by Persi Diaconis:
  "Persi Diaconis On coincidences"
  https://www.youtube.com/watch?v=EVGATVaKK7M&list=PL7LlTeoYa2BuEpOs9SOXnSwVldjkWZJSt
  @0:40ff

  This is also mentioned in https://www.edge.org/response-detail/10211
  """
  Chance as an Unseen Force

  Eighty-three years later Carl Jung published a similar idea in his well-known essay 
  "Synchronicity, An Acausal Connecting Principle." He postulated the existence of a 
  hidden force that is responsible for the occurrence of seemingly related events that 
  otherwise appear to have no causal connection. The initial story of the six fish 
  encounters is Jung's, taken from his book. He finds this string of events unusual, 
  too unusual to be ascribable to chance. He thinks something else must be going on—
  and labels it the acausal connecting principle.

  Persi Diaconis, Stanford Professor and former professor of mine, thinks critically 
  about Jung's example: suppose we encounter the concept of fish once a day on average 
  according to what statisticians call a "Poisson process" (another fish reference!). 
  The Poisson process is a standard mathematical model for counts, for example radioactive 
  decay seems to follow a Poisson process. The model presumes a certain fixed rate at 
  which observations appear on average and otherwise they are random. So we can consider 
  a Poisson process for Jung's example with a long run average rate of one observation 
  per 24 hours and calculate the probability of seeing six or more observations of 
  fish in a 24 hour window. Diaconis finds the chance to be about 22%. Seen from this 
  perspective, Jung shouldn't have been surprised.
  """

  I cannot reproduce Diaconi's result of a probability of 22% with >= 6 fish 
  stories. All four models below give a probability of about 10%, which 
  still suggests that Jung shouldn't be surprised.


  This program is (partly) a port of my WebPPL program jungs_fish_stories.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

#|
  First a very simple model:
  We assume a poisson process with rate 1.
  What is the probability that there are 6 or more fish stories during this day:
  Quite unlikely:
  
  variable : num_fish_stories
  1: 0.36878000000000005
  0: 0.36733000000000005
  2: 0.18379000000000004
  3: 0.061430000000000005
  4: 0.015060000000000002
  5: 0.0030000000000000005
  6: 0.0005200000000000001
  7: 6.000000000000001e-5
  8: 3.0000000000000004e-5
  mean: 0.9996700000000002
  HPD interval (0.84): 0..2

  variable : p
  #f: 0.99939
  #t: 0.0006100000000000001
  mean: 0.0006100000000000001

  variable : p2
  0.1069532671647: 1.0
  mean: 0.1069532671647

  The exact probability of >= 6 occurrences on a day is
  > (- 1 (dist-cdf (poisson-dist 1) 5))
  0.0005941848175816666

  This is just for one day. Since the range is over 6 months (we assume 30 days 
  in each month), the probability is
    0.0005941848175816666 * 6*30 = 0.106953267164699988
  i.e. about 10.7%, as shown in the p2 variable.

|#
(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num_fish_stories (poisson 1))
   (define p (>= num_fish_stories 6))

   (define p2 (* 6 30 (- 1 (dist-cdf (poisson-dist 1) 5))))
   
   (list num_fish_stories
         p
         p2
    )
   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list  "num_fish_stories"
                       "p"
                       "p2"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )




#|

  Here is a more elaborate model where we simulate each of the 30 days
  a day with a poisson distribution with rate 1, and checks

  The probability of >= 6 fish stories in some of the 30 days is about 
  10.4%, still far from Diaconis' 22%:

  variable : p
  #f: 0.896
  #t: 0.104
  mean: 0.104

|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num_days (* 6 30)) ; number of days
    
   ;; Number of fish stories per day
   (define (fish_stories day) (poisson 1))

   ; Are day any days with >= 6 fish stories?
   (define p
     (> (for/sum ((day num_days))
          (boolean->integer (>= (fish_stories day) 6)) ) 0))
   
   (list p)
  
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list "p"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
  A simpler version of model 2 is to skip the 24 window
  and just check each day. This is better than model2 but
  10.07% is quite a bit from from Diaconis' 22%.
  (I added max fish stories per day just for fun. :-)) 

  variable : num_more_then_6_fish_stories_per_day
  0: 0.8992900000000001
  1: 0.09579000000000003
  2: 0.00477
  3: 0.00015000000000000001
  mean: 0.10578000000000004
  HPD interval (0.84): 0..0

  variable : max_fish_stories_per_day
  4: 0.48477000000000003
  5: 0.38322000000000006
  6: 0.08599000000000001
  3: 0.03130000000000001
  7: 0.012910000000000003
  8: 0.0015900000000000003
  9: 0.00019000000000000004
  10: 3.0000000000000004e-5
  mean: 4.570120000000001
  HPD interval (0.84): 4..5

  variable : p
  #f: 0.8992900000000001
  #t: 0.10071000000000002
  mean: 0.10071000000000002

|#
(define (model3)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num_days (* 6 30)) ; 6 months
   (define fish_stories_per_day (for/list ([i num_days]) (poisson 1)))
   (define more_than_6_fish_stories_per_day
     (for/list ([i num_days])
       (boolean->integer (>= (list-ref fish_stories_per_day i) 6))))
   
   (define max_fish_stories_per_day (apply max fish_stories_per_day))
   (define num_more_then_6_fish_stories_per_day (sum more_than_6_fish_stories_per_day))
   (define p (> num_more_then_6_fish_stories_per_day 0))

   (list num_more_then_6_fish_stories_per_day
         max_fish_stories_per_day
         p
    )
   )
)

(displayln "\nModel 3")
(show-marginals (model3)
                (list "num_more_then_6_fish_stories_per_day"
                      "max_fish_stories_per_day"
                      "p"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

#|
  And a fourth model: For each chunks of 30 days, check if there are any occurrence of >= 6.
  About 10%, still too low compared to Diaconis' 22%: 

  variable : max_fish_stories_per_day
  4: 0.503
  5: 0.369
  6: 0.08400000000000002
  3: 0.033
  7: 0.011
  mean: 4.537000000000001
  HPD interval (0.84): 4..5

  variable : p
  #f: 0.905
  #t: 0.09500000000000003
  mean: 0.09500000000000003


|#
(define (model4)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num_days (* 6 30)) ; 6 months
   (define fish_stories_per_day (for/list ([i num_days]) (poisson 1)))
   
   (define more_than_6_fish_stories_per_day
     (for/list ([c (chunks-of fish_stories_per_day 30)])
       (boolean->integer (> (for/sum ([cc c]) (boolean->integer (>= cc 6) )) 0) )))
   
   (define max_fish_stories_per_day (apply max fish_stories_per_day))
   (define num_more_then_6_fish_stories_per_day (sum more_than_6_fish_stories_per_day))
   (define p (> num_more_then_6_fish_stories_per_day 0))

   (list max_fish_stories_per_day
         p
    )
   )
)

(displayln "\nModel 4")
(show-marginals (model4)
                (list "max_fish_stories_per_day"
                      "p"
                       )
                #:num-samples 2000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


