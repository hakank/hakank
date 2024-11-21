#| 

  Rope length in Racket/Gamble 

  From Wolfram U's Intro to Probability 
  https://www.wolframcloud.com/obj/online-courses/introduction-to-probability/joint-distributions.html
  """
  A rope of length 1 is cut according to a 15-10-5 Dirichlet distribution.
  ...
  Find the expected length of the rope parts:
  """

  Mathematica's solution:
  dist = DirichletDistribution[{15, 10, 5}]
  Table[Mean[MarginalDistribution[dist, i]], {i, 2}]
  {1/2, 1/3}


  variable : r1
  mean: 0.4995861374888271

  variable : r2
  mean: 0.3333243123642886

  variable : r3
  mean: 0.1670895501469784

  variable : len
  mean: 1.000000000000001


  Compare with 
  > (dist-mean (dirichlet-dist (vector 15 10 5)))
  '#(0.5 0.3333333333333333 0.16666666666666666)

  Note that the numbers 15,10,5 represents the expected proportions:
  > (define *x* '(15 10 5))
  > (map (lambda (v) (/ v (sum *x*))) *x*)
  '(1/2 1/3 1/6)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define rope (vector->list (dirichlet (vector 15 10 5))))
   
   (list (first rope)
         (second rope)
         (third rope)
         (sum rope)
         )

   )
)

(show-marginals (model)
                (list  "r1"
                       "r2"
                       "r3"
                       "len"
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
                    ; #:thin 0
                    )


