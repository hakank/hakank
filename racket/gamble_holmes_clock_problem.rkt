#| 

  Holmes clock problem in Racket.Gamble 

  From "Introduction to Probability" av Grinstead & Snell, page 92
  """
  Mr. Wimply Dimple, one of London's most prestigious watch makers, has come to 
  Sherlock Holmes in a panic, having discovered that someone has been producing and 
  selling crude counterfeits of his best selling watch. The 16 counterfeits so far 
  discovered bear stamped numbers, all of which fall between 1 and 56, and Dimple is 
  anxious to know the extent of the forger's work. All present agree that it seems 
  reasonable to assume that the counterfeits thus far produced bear consecutive 
  numbers from 1 to whatever the total number is. "Chin up, Dimple," opines Dr. Watson.
  "I shouldn't worry overly much if I were you; the Maximum Likelihood Principle, 
  which estimates the total number as precisely that which gives the highest 
  probability for the series of numbers found, suggests that we guess 56 itself as the
  total. Thus, your forgers are not a big operation, and we shall have them safely 
  behind bars before your business suffers significantly." "Stuff, nonsense, and 
  bother your fancy principles, Watson," counters Holmes. "Anyone can see that, of 
  course, there must be quite a few more than 56 watches - why the odds of our having 
  discovered precisely the highest numbered watch made are laughably negligible. A 
  much better guess would be twice 56." 
  """

  Let's simulate this by an urn model were we have two urns:
  - urn 1 which have 56 balls numbered 1..56
  - urn 2 which have 2*56 = 112 balls numbered 1..112

  The probability of selecting each of the urns are 1/2.

  Using Bayes formula:

      p(ball from urn 1) = 0.5
      p(ball from urn 2) = 0.5

      p(number 56 is drawn|urn=urn 1) = 1/56
      p(nummer 56 is drawn|urn=urn 2) = 1/112

  What are the probabilities:
    p(urn 1|number 56 was drawn)
    p(urn 2|number 56 was drawn)

  p(urn 1|number 56 was drawn) = p(urn 1 /\ number 56) / p(number 56)

                          = p(number 56|urn 1) * p(urn 1)
                            ------------------------------
                            p(number 56|urn 1) * p(urn 1) + p(number 56|urn 2)*p(urn 2)

                          = 1/56 * 0.5
                            ----------
                            1/56 * 0.5 + 1/112*0.5

                          = (1/56 * 0.5)/(1/56 * 0.5 + 1/112*0.5) 
                          = 0.6666667

  p(urn 2|number 56 was drawn) = p(urn 2 /\ number 56) / p(number 56)

                          = p(number 56|urn 2) * p(urn 2)
                            ------------------------------
                            p(number 56|urn 1) * p(urn 1) + p(number 56|urn 2)*p(urn 2)

                          = 1/112 * 0.5
                            ----------
                            1/56 * 0.5 + 1/112*0.5

                          = (1/112 * 0.5)/(1/56 * 0.5 + 1/112*0.5) 
                          = 0.3333333


  This model gives the following result which confirms the calculations (or the other way around :-)

  Distributions of variable urn
    urn1       =>     668  (0.668000)
    urn2       =>     332  (0.332000)
 
  I.e. if we draw ball number 56 it's more likely that it was from urn number 1 (with balls 1..56) than 
  from urn 2 (with balls 1..112), 66% vs 33%

  Here are some more experiments were we increments the number of balls in urn2:

  (num_balls1 56 num_balls2 112 ball_obs 56)
  var : urn
  urn1: 2/3 (0.6666666666666666)
  urn2: 1/3 (0.3333333333333333)

  (num_balls1 56 num_balls2 168 ball_obs 56)
  var : urn
  urn1: 3/4 (0.75)
  urn2: 1/4 (0.25)

  (num_balls1 56 num_balls2 224 ball_obs 56)
  var : urn
  urn1: 4/5 (0.8)
  urn2: 1/5 (0.2)

  (num_balls1 56 num_balls2 280 ball_obs 56)
  var : urn
  urn1: 5/6 (0.8333333333333334)
  urn2: 1/6 (0.16666666666666666)

  (num_balls1 56 num_balls2 336 ball_obs 56)
  var : urn
  urn1: 6/7 (0.8571428571428571)
  urn2: 1/7 (0.14285714285714285)

  (num_balls1 56 num_balls2 392 ball_obs 56)
  var : urn
  urn1: 7/8 (0.875)
  urn2: 1/8 (0.125)

  (num_balls1 56 num_balls2 448 ball_obs 56)
  var : urn
  urn1: 8/9 (0.8888888888888888)
  urn2: 1/9 (0.1111111111111111)

  (num_balls1 56 num_balls2 504 ball_obs 56)
  var : urn
  urn1: 9/10 (0.9)
  urn2: 1/10 (0.1)

  (num_balls1 56 num_balls2 560 ball_obs 56)
  var : urn
  urn1: 10/11 (0.9090909090909091)
  urn2: 1/11 (0.09090909090909091)


  This is a port of my WebPPL model holmes_clock_problem.wppl

  But cf with Mosteller's Locomitive problem (gamble_locomotive_problem.rkt) with 56 as the locomotive number observed:
  Max-int  Estimate 
  -----------------
   100      75.40107023621401
   200     112.69364171265156 
   500     202.54329682025934 
  1000     327.4619016746249
  2000     544.0112652172352

  (Mosteller's estimate would be 2*(56-1)+1 = 111)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model num_balls1 num_balls2 ball_obs)
  (show2 "num_balls1" num_balls1 "num_balls2" num_balls2 "ball_obs" ball_obs)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define urn1 0)
   (define urn2 1)
        
   (define urn (categorical-vw2 (vector 1/2 1/2) (vector "urn1" "urn2")))
   (define ball (if (eq? urn "urn1") (add1 (random-integer num_balls1)) (add1 (random-integer num_balls2))))
        
   (observe/fail (= ball ball_obs))
        
   (list urn
        )

   )
)

(define num_balls1 56)
(define num_balls2 112)
(define ball_obs 56)
(show-marginals (model num_balls1 num_balls2 ball_obs)
                  (list  "urn"
                         )
                  #:num-samples 1000
                  #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )

(for ([num_balls2 8])
  (show-marginals (model num_balls1 (* 56 (+ num_balls2 3)) ball_obs)
                  (list  "urn"
                         )
                  #:num-samples 1000
                  #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )
  )


