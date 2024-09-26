#| 

  Beta binomial urn model in Racket.Gamble 

  https://en.wikipedia.org/wiki/Beta-binomial_distribution
  """
  The beta-binomial distribution can also be motivated via an urn model for positive 
  integer values of α and β, known as the Pólya urn model. Specifically, imagine an 
  urn containing α red balls and β black balls, where random draws are made. If a red ball 
  is observed, then two red balls are returned to the urn. Likewise, if a black ball is 
  drawn, then two black balls are returned to the urn. If this is repeated n times, 
  then the probability of observing x red balls follows a beta-binomial distribution 
  with parameters n, α and β.

  If the random draws are with simple replacement (no balls over and above the observed ball 
  are added to the urn), then the distribution follows a binomial distribution and if 
  the random draws are made without replacement, the distribution follows a 
  hypergeometric distribution.
  """

  Here is a simple example: 
  - number of white balls = number of black balls = 1
  - number of balls to add if a white or black ball is drawn = 1 (of the same color)
  - 10 draws

  (num_white: 1 num_black: 1 num_draws: 10 num_add_white: 1 num_add_black: 1)

  Model 1
  var : num_white_obs
  mean: 5 (5.0)

  var : num_black_obs
  mean: 5 (5.0)

  var : num_white_left
  mean: 6 (6.0)

  var : num_black_left
  mean: 6 (6.0)

  var : num_runs
  mean: 10 (10.0)

  var : all
  (3 7 4 8): 1/11 (0.09090909090909091)
  (6 4 7 5): 1/11 (0.09090909090909091)
  (2 8 3 9): 1/11 (0.09090909090909091)
  (1 9 2 10): 1/11 (0.09090909090909091)
  (9 1 10 2): 1/11 (0.09090909090909091)
  (4 6 5 7): 1/11 (0.09090909090909091)
  (8 2 9 3): 1/11 (0.09090909090909091)
  (10 0 11 1): 1/11 (0.09090909090909091)
  (5 5 6 6): 1/11 (0.09090909090909091)
  (7 3 8 4): 1/11 (0.09090909090909091)
  (0 10 1 11): 1/11 (0.09090909090909091)

  Model 2:


  var : beta_binomial
  mean: 5.001979999999996

  var : polya_eggenberg
  mean: 5.005579999999996

  var : polya
  mean: 4.997569999999996


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define num_white 1)
(define num_black 1)
(define num_draws 10)
(define num_add_white 1)
(define num_add_black 1)


#|
  A general Urn model (2 balls)
  Given
   - number of white balls
   - number of black balls
   - number of draws
   - number of white balls to add if we draw a white ball
   - number of black balls to add if we draw a black ball
  Returns
   - (number of observed white balls, number of observed black balls)
   - (number of left white balls, number of left black balls)

  If the number of balls to is negative, we stop if any of the balls reach 0 (or below).


|#
(define (urn_model num_white num_black num_draws num_add_white num_add_black)
  (show2 "num_white:" num_white "num_black:" num_black "num_draws:" num_draws "num_add_white:" num_add_white "num_add_black:" num_add_black)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ;; Return the number of observed white and black balls and number of white/black
   ;; balls that are left.
   (define (f num_white num_black num_white_observed num_black_observed i)
     (if (or (= i num_draws) (<= num_white 0) (<= num_black 0))
         (values num_white_observed num_black_observed
               (max num_white 0)
               (max num_black 0)
               i)
         (let* ((p_white (/ num_white (+ num_white num_black))) ;; Probability of white ball
                (pick (flip p_white)))
           (if pick 
               (f (+ num_white num_add_white)
                  num_black
                  (add1 num_white_observed)
                  num_black_observed
                  (add1 i))
               (f num_white
                  (+ num_black num_add_black)
                  num_white_observed
                  (add1 num_black_observed)
                  (add1 i))
               )
           )
         )
     )

   ; (define res (f num_white num_black 0 0 0))

   (let-values (((num_white_obs num_black_obs num_white_left num_black_left num_runs) (f num_white num_black 0 0 0)))
     (list num_white_obs
           num_black_obs
           num_white_left
           num_black_left
           num_runs
           (list num_white_obs num_black_obs num_white_left num_black_left)
           ))
   
   ; res
   
   )
  )

(show-marginals (urn_model num_white num_black num_draws num_add_white num_add_black)
                (list  "num_white_obs"
                       "num_black_obs"
                       "num_white_left"
                       "num_black_left"
                       "num_runs"
                       "all"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


#|
  These "urn distribution" are all based on beta_binomial_dist().
  They all give the number of observed white balls.

  
|#


(displayln "urn_model_dists")
(define (urn_model_dists num_white num_black num_draws num_add_white num_add_black)
  (show2 "num_white:" num_white "num_black:" num_black "num_draws:" num_draws "num_add_white:" num_add_white "num_add_black:" num_add_black)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; Check with beta_binomial_dist, only valid when both adds are 1.
   (define beta_binomial_v
     ;; (if (and (> num_add_white 0) (= num_add_white num_add_black))
     ;;     (beta_binomial num_draws (/ num_white num_add_white) (/ num_black num_add_white))
     ;;     -1))
     (beta_binomial num_draws (/ num_white num_add_white) (/ num_black num_add_white)))

   ;; Polya Eggenberg distribution: only valid if num_add_white == num_add_black
   (define polya_eggenberg_v
     (if (and (> num_add_white 0) (= num_add_white num_add_black))
         (polya_eggenberg num_draws num_white num_black num_add_white)
         -1))

   ;; Polya distribution. Only valid if both add's are the same
   (define polya_v
     (if (and (> num_add_white 0) (= num_add_white num_add_black))
         (polya num_draws num_white num_black num_add_white)
         1))
        
   (list beta_binomial_v
         polya_eggenberg_v
         polya_v
        )
   )
  )

(show-marginals (urn_model_dists num_white num_black num_draws num_add_white num_add_black)
                (list  "beta_binomial"
                       "polya_eggenberg"
                       "polya"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


