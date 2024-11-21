#| 

  Duck Hunter problem in Racket/Gamble 

  From Siegrist "Probability Mathematical Statisics and Stochastic Processes", page 
  """
  Suppose that there are 5 duck hunters, each a perfect shot. A flock of 10 ducks fly over, and each
  hunter selects one duck at random and shoots. Find each of the following:
  1. The probability density function of the number of ducks that are killed.
  2. The mean of the number of ducks that are killed.
  3. The standard deviation of the number of ducks that are killed.
  """

  variable : num_killed
  4: 63/125 (0.504)
  5: 189/625 (0.3024)
  3: 9/50 (0.18)
  2: 27/2000 (0.0135)
  1: 1/10000 (0.0001)
  mean: 40951/10000 (4.0951)
  Min: 1 Mean: 4.1052 Max: 5 Variance: 0.52573296 Stddev: 0.7250744513496528


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define duck_hunters 5)
   (define ducks 10)

   (define shoots (for/list ([i duck_hunters]) (random-integer ducks)))
   (define num_killed (length (remove-duplicates shoots)))

   (list num_killed)
   
   )
)

(show-marginals (model)
                (list "num_killed"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


