#| 

  Selling candies in Racket/Gamble 

  From https://en.wikipedia.org/wiki/Negative_binomial_distribution
  """
  Selling candy
  Pat Collis is required to sell candy bars to raise money for the 6th grade field trip. 
  Pat is (somewhat harshly) not supposed to return home until five candy bars have been sold. So 
  the child goes door to door, selling candy bars. At each house, there is a 0.6 probability of selling 
  one candy bar and a 0.4 probability of selling nothing.

  What's the probability of selling the last candy bar at the nth house?
  ...
  What's the probability that Pat finishes on the tenth house?
    f(10)=0.1003290624
  """

  Almost exact (enumerate #:limit 1e-10)

  variable : y
  2: 0.1866240000206039
  3: 0.17418240001923022
  1: 0.1555200000171699
  4: 0.1393459200153843
  5: 0.10032906241107646
  ...
  31: 1.877654234502705e-9
  32: 8.449444055262228e-10
  33: 3.789447636905083e-10
  34: 1.6941060023813152e-10
  35: 2.528891853829165e-11
  mean: 3.3333333297294714

  variable : p
  #f: 0.8996709375889236
  #t: 0.10032906241107646
  mean: 0.10032906241107646

  Exact:
  (negative_binomial_mean (- num_houses num_candies) prob_of_selling)): 10/3 (3.3333333333333335)
  (negative_binomial_pdf (- num_houses num_candies) prob_of_selling)) num_candies: 979776/9765625 (0.1003290624)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-10
  
   (define num_houses 10)
   (define num_candies 5)
   (define prob_of_selling 6/10)
   
   (define y (negative_binomial (- num_houses num_candies) prob_of_selling))
   (define p (= y num_candies))
   
   (list y
         p
         )

   )
)

(show-marginals (model)
                (list  "y"
                       "p"
                       ))

(newline)
(displayln "Exact:")
(let* ([num_houses 10]
       [num_candies 5]
       [prob_of_selling 6/10]
       [v1 (negative_binomial_mean (- num_houses num_candies) prob_of_selling)]
       [v2 (negative_binomial_pdf (- num_houses num_candies) prob_of_selling num_candies)])
  (displayln (format "(negative_binomial_mean (- num_houses num_candies) prob_of_selling)): ~a (~a)" v1 (* 1.0 v1)))
  (displayln (format "(negative_binomial_pdf (- num_houses num_candies) prob_of_selling)) num_candies: ~a (~a)" v2 (* 1.0 v2) ))
  )
(newline)
