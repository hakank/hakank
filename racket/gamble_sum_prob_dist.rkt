#| 

  sum distribution in Racket/Gamble 

  Distribution of summing discrete uniform numbers to a specific sum.

  Inspired by Fletcher Thompson "What’s the Probability Ten Dice Add Up To 12?"
  https://medium.com/puzzle-sphere/whats-the-probability-ten-dice-add-up-to-12-83f637205505

  Probability of getting the sum 12 when throwing 10 fair dice:
  55/60466176 (9.095994428356111e-7, about 1 in 1099385.018)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(displayln "Probability of getting the sum 12 when throwing 10 fair dice:")
(sum_prob_pdf 1 6 10 12)
(* 1.0 (sum_prob_pdf 1 6 10 12))
(newline)

#|

  Probability that throwing 4 dice give the sum 12 (enumerate)

  variable : ss
  14: 73/648 (0.11265432098765432)
  13: 35/324 (0.10802469135802469)
  15: 35/324 (0.10802469135802469)
  12: 125/1296 (0.09645061728395062)
  16: 125/1296 (0.09645061728395062)
  11: 13/162 (0.08024691358024691)
  17: 13/162 (0.08024691358024691)
  10: 5/81 (0.06172839506172839)
  18: 5/81 (0.06172839506172839)
  9: 7/162 (0.043209876543209874)
  19: 7/162 (0.043209876543209874)
  8: 35/1296 (0.02700617283950617)
  20: 35/1296 (0.02700617283950617)
  7: 5/324 (0.015432098765432098)
  21: 5/324 (0.015432098765432098)
  6: 5/648 (0.007716049382716049)
  22: 5/648 (0.007716049382716049)
  5: 1/324 (0.0030864197530864196)
  23: 1/324 (0.0030864197530864196)
  4: 1/1296 (0.0007716049382716049)
  24: 1/1296 (0.0007716049382716049)
  mean: 14 (14.0)
  Min: 4 Mean: 14.003507 Max: 24 Variance: 11.688832700951 Stddev: 3.418893490729274

  variable : p
  #f: 1171/1296 (0.9035493827160493)
  #t: 125/1296 (0.09645061728395062)
  mean: 125/1296 (0.09645061728395062)
  Min: 0 Mean: 0.096205 Max: 1 Variance: 0.086949597975 Stddev: 0.29487217226282986

  variable : p-theoretical
  125/1296: 1 (1.0)
  mean: 125/1296 (0.09645061728395062)
  Min: 125/1296 Mean: 0.09645061728395062 Max: 125/1296 Variance: 0 Stddev: 0

  variable : stddev-theoretical
  3.415650255319866: 1 (1.0)
  mean: 3.415650255319866
 Min: 3.415650255319866 Mean: 3.4156502553207257 Max: 3.415650255319866 Variance: 0.0 Stddev: 0.0

|#
(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 4)
   (define s 12)
   (define x (for/list ([i n]) (add1 (random-integer 6))))
   (define ss (sum x))
   (define p (= ss s))

   (define p-theoretical (sum_prob_pdf 1 6 n s))
   (define stddev-theoretical (sqrt (sum_prob_variance 1 6 n)))
   
   (list ss
         p
         p-theoretical
         stddev-theoretical
         )

   )
)

(show-marginals (model)
                (list  "ss"
                       "p"
                       "p-theoretical"
                       "stddev-theoretical"
                       
                     )
                    #:num-samples 1000000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )

#|
  Probability of getting the sum 12 when throwing 10 dice.
  Enumerate is too slow for this.

|#
