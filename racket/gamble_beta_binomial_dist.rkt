#| 

  Beta-binomial dist in Racket.Gamble 

  https://en.wikipedia.org/wiki/Beta-binomial_distribution#Generating_beta_binomial-distributed_random_variables
  """
  The beta-binomial distribution is the binomial distribution in which the 
  probability of success at each of n trials is not fixed but randomly drawn 
  from a beta distribution. It is frequently used in Bayesian statistics, 
  empirical Bayes methods and classical statistics to capture overdispersion 
  in binomial type distributed data.

  The beta-binomial is a one-dimensional version of the Dirichlet-multinomial 
  distribution as the binomial and beta distributions are univariate versions 
  of the multinomial and Dirichlet distributions respectively. The special case 
  where α and β are integers is also known as the negative hypergeometric distribution. 

  ...

  To draw a beta-binomial random variate X ~ BetaBin(n,a,b) 
  simply draw p ~ Beta(a,b) and then draw X ~ B(n,p)
  """

  Here an example of (beta_binomial 10 3)
  var : x
  10: 0.20400000000000001
  11: 0.19100000000000003
  9: 0.18500000000000003
  8: 0.12100000000000001
  12: 0.11200000000000002
  7: 0.10500000000000001
  6: 0.043000000000000003
  5: 0.021000000000000005
  4: 0.014000000000000002
  3: 0.0030000000000000005
  2: 0.0010000000000000002
  mean: 9.283000000000001

  See below for some more examples.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define a 10)
   (define b 3)
   (define x (beta_binomial 12 a b))
   
   (list x)
   
   )
)

(show-marginals (model)
                (list  "x")
                #:num-samples 1000)



#|
  From https://www.acsu.buffalo.edu/~adamcunn/probability/betabinomial.html
  """
  The probability each year that an apple contains a worm is a beta(0.5, 8) 
  random variable. 100 apples are picked one year. Let X be the number 
  containing worms.
  """

  var : x
  0: 0.26773
  1: 0.12733
  2: 0.08785
  3: 0.06767
  4: 0.0574
  ...
  68: 2e-5
  71: 2e-5
  70: 1e-5
  73: 1e-5
  76: 1e-5
  mean: 5.8567399999999985


|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define a 0.5)
   (define b 8)
   (define x (beta_binomial 100 a b))
   
   (list x)
   
   )
)

(displayln "\nModel2")
(show-marginals (model2)
                (list  "x")
                #:num-samples 100000
                #:truncate-output 5)

; A simpler way to just get the mean value
; (* 1.0 (avg (repeat (lambda () (beta_binomial 100 0.5 8)) 1000)))


#|
  From https://www.acsu.buffalo.edu/~adamcunn/probability/betabinomial.html
  
  """
  The probability that a random student can answer an exam question 
  correctly has a beta(3, 2) distribution. Let X be the number of correct 
  answers on a 20 question exam.
  """

  var : x
  12: 0.07941
  13: 0.0785
  15: 0.07779
  14: 0.07641
  11: 0.07279
  ...
  20: 0.02134
  3: 0.01782
  2: 0.01057
  1: 0.00526
  0: 0.00195
  mean: 12.013739999999999

|#
(define (model3)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define a 3)
   (define b 2)
   (define x (beta_binomial 20 a b))
   
   (list x)
   
   )
)

(displayln "\nModel3")
(show-marginals (model3)
                (list "x")
                #:num-samples 100000
                #:truncate-output 5)


#|
  From https://www.acsu.buffalo.edu/~adamcunn/probability/betabinomial.html  
  """
  The probability that an archer in the Night's Watch can hit a white 
  walker has a beta(10, 3) distribution. A randomly chosen archer fires 
  36 arrows. Let X be the number of white walkers hit.
  """

  var : x
  30: 0.08619
  29: 0.08389
  31: 0.08216
  28: 0.08108
  32: 0.07575
  ...
  9: 0.00023
  8: 0.00016
  7: 5e-5
  6: 2e-5
  5: 1e-5
  mean: 27.714340000000007


|#
(define (model4)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define a 10)
   (define b 3)
   (define x (beta_binomial 36 a b))
   
   (list x)
   
   )
)

(displayln "\nModel4")
(show-marginals (model4)
                (list "x")
                #:num-samples 100000
                #:truncate-output 5
                )

