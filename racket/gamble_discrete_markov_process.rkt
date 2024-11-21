#| 

  Discrete Markov Process in Racket/Gamble 

  From Mathematica DiscreteMarkovProcess

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


#|
  From Mathematica DiscreteMarkovProcess
  """
  A simple weather model is: given that it rains today, it will rain tomorrow 
  with probability 0.7; and given that it does not rain today, it will rain tomorrow 
  with probability 0.4. Given that it is raining today, represent this model with a 
  discrete Markov process and find the probability that it will rain four days from 
  now. 

  ...

  weather = DiscreteMarkovProcess[1, {{0.7, 0.3}, {0.4, 0.6}}];

  The probability of rain in four days:
  Probability[rain[4] == 1, rain in weather]
  -> 
  0.5749
  """

  variable : last-day
  0: 5749/10000 (0.5749)
  1: 4251/10000 (0.4251)
  mean: 4251/10000 (0.4251)

  variable : p
  #t: 5749/10000 (0.5749)
  #f: 4251/10000 (0.4251)
  mean: 5749/10000 (0.5749)

  variable : a
  (0 0 0 0 0): 2401/10000 (0.2401)
  (0 0 0 0 1): 1029/10000 (0.1029)
  (0 0 0 1 1): 441/5000 (0.0882)
  (0 0 1 1 1): 189/2500 (0.0756)
  (0 1 1 1 1): 81/1250 (0.0648)
  (0 0 1 0 0): 147/2500 (0.0588)
  (0 0 0 1 0): 147/2500 (0.0588)
  (0 1 0 0 0): 147/2500 (0.0588)
  (0 0 1 1 0): 63/1250 (0.0504)
  (0 1 1 0 0): 63/1250 (0.0504)
  (0 1 1 1 0): 27/625 (0.0432)
  (0 1 0 0 1): 63/2500 (0.0252)
  (0 0 1 0 1): 63/2500 (0.0252)
  (0 1 1 0 1): 27/1250 (0.0216)
  (0 1 0 1 1): 27/1250 (0.0216)
  (0 1 0 1 0): 9/625 (0.0144)

  variable : p-theoretical
  5749/10000: 1 (1.0)
  mean: 5749/10000 (0.5749)

|#
(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define rain 0)
   (define not-rain 1)
   
   (define transitions '((7/10 3/10)
                         (4/10 6/10)
                         ))
   
   
   (define initial-state '(1 0)) ; It rains today
   
   (define n 4) ; Does it rain 4 days from now

   (define a (markov-chain transitions initial-state (add1 n) ))
   (define last-day (last a))

   (define p (= last-day rain))

   ; Theoretical probability
   (define p-theoretical (discrete_markov_process_pdf transitions initial-state n rain))
  
   (list last-day
         p
         p-theoretical
         a
         )

   )
)

(displayln "\nModel 1")
(show-marginals (model1)
                (list  "last-day"
                       "p"
                       "p-theoretical"
                       "a"
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
