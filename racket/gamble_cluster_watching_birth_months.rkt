#| 

  Cluster watching in birthday months in Racket.Gamble 

  From 
  John Brignell "Of birthdays and clusters"
  https://www.numberwatch.co.uk/of_birthdays_and_clusters.htm
  """
  As we gratefully witness the dying fall of yet another silly season, it is interesting 
  to note how often birthdays provide the basis of the most popular fallacies. In August 2001 
  a classic of the genre appeared when the New Scientist announced that researchers in 
  Scotland had established that anorexics are likely to be have been June babies. The team 
  studied 446 women who had been diagnosed as anorexic and observed that 30% more than average 
  were born in June. As the monthly average of births is about 37, we deduce that the June 
  number must have been 48. At first sight this looks like a significant result (at least 
  by epidemiological standards) since the probability of getting 48 or more in a random 
  month is about 3%. But that is not what they are doing! They are making twelve such 
  selections and then picking the biggest. Application of the theory of the statistics of 
  extremes tells us that the probability of the largest of twelve such selections being 
  48 or greater is 30%, which is not significant even by epidemiological standards.
  """

  Compare with my R code (with Swedish comments) at
  http://www.hakank.org/sims/simulering.html "Cluster watching"

  This model calculates the min, mean, and max value of 446 random months, as well as
  two probabilities:
  - p: what is the probability that the max value is more than 30% of the mean value
  - p2: how many months has a value that is more than 30% of the mean value.
    (There's about 2% chance that as many as 3 months has such as value)

  var : minVal
  28: 0.1619999999999999
  29: 0.15499999999999992
  27: 0.1359999999999999
  30: 0.10999999999999993
  26: 0.10199999999999994
  31: 0.08199999999999995
  25: 0.06399999999999996
  24: 0.05299999999999997
  32: 0.03599999999999998
  23: 0.03299999999999998
  22: 0.02999999999999999
  21: 0.01299999999999999
  33: 0.008999999999999994
  20: 0.006999999999999996
  34: 0.0039999999999999975
  18: 0.0029999999999999983
  35: 0.0009999999999999994
  mean: 27.57499999999998
  Credible interval (0.94): 22..32
  Percentiles:
  (0.01 20)
  (0.025 21)
  (0.1 24)
  (0.05 22)
  (0.25 26)
  (0.5 28)
  (0.75 30)
  (0.84 30)
  (0.9 31)
  (0.95 32)
  (0.975 32)
  (0.99 33)
  (0.999 34)
  Histogram:
  14: 31 
  16: 19 
  18: 2  
  19: 1  
  20: 1  
  21: 1  
  22: 3  
  23: 4  
  24: 7  
  25: 17 
  26: 25 
  27: 37 
  28: 54 
  29: 74 
  30: 90 
  31: 114
  32: 151
  33: 164
  34: 135
  35: 70 

  var : meanVal
  223/6: 1.0000000000000004
  mean: 37.16666666666668

  var : maxVal
  46: 0.1399999999999999
  47: 0.13299999999999992
  45: 0.10899999999999994
  48: 0.10699999999999993
  44: 0.09799999999999995
  49: 0.09599999999999995
  50: 0.06299999999999996
  51: 0.04799999999999997
  43: 0.04599999999999997
  52: 0.038999999999999986
  53: 0.03199999999999998
  42: 0.028999999999999988
  54: 0.020999999999999987
  55: 0.011999999999999992
  41: 0.007999999999999995
  58: 0.005999999999999997
  57: 0.005999999999999996
  56: 0.0039999999999999975
  59: 0.0009999999999999994
  60: 0.0009999999999999994
  62: 0.0009999999999999994
  mean: 47.48299999999996
  Credible interval (0.94): 42..53
  Percentiles:
  (0.01 42)
  (0.025 42)
  (0.1 44)
  (0.05 43)
  (0.25 45)
  (0.5 47)
  (0.75 49)
  (0.84 50)
  (0.9 51)
  (0.95 53)
  (0.975 54)
  (0.99 56)
  (0.999 58)
  Histogram:
  41: 6  
  42: 22 
  43: 70 
  44: 90 
  45: 132
  46: 124
  47: 109
  48: 110
  49: 108
  50: 77 
  51: 62 
  52: 29 
  53: 18 
  54: 23 
  55: 8  
  56: 6  
  57: 2  
  58: 3  
  60: 1  

  var : p
  #f: 0.6699999999999998
  #t: 0.33
  mean: 0.33

  var : p2
  0: 0.6699999999999998
  1: 0.29999999999999993
  2: 0.029999999999999992
  mean: 0.35999999999999993
  Credible interval (0.94): 0..1
  Percentiles:
  (0.01 0)
  (0.025 0)
  (0.1 0)
  (0.05 0)
  (0.25 0)
  (0.5 0)
  (0.75 1)
  (0.84 1)
  (0.9 1)
  (0.95 1)
  (0.975 2)
  (0.99 2)
  (0.999 2)
  Histogram:
  0: 663
  1: 299
  2: 38 

  var : gt-months
  (): 0.6649999999999998
  (6): 0.03599999999999999
  (5): 0.030999999999999996
  (10): 0.030999999999999996
  (0): 0.027999999999999997
  (1): 0.027999999999999994
  (9): 0.02699999999999999
  (3): 0.02499999999999999
  (2): 0.022999999999999996
  (7): 0.021999999999999995
  (11): 0.018999999999999993
  (4): 0.01699999999999999
  (8): 0.01599999999999999
  (2 4): 0.0019999999999999987
  (5 9): 0.0019999999999999987
  (0 2): 0.0019999999999999987
  (1 9): 0.0009999999999999994
  (1 3): 0.0009999999999999994
  (5 8): 0.0009999999999999994
  (5 7): 0.0009999999999999994
  (7 8): 0.0009999999999999994
  (6 9): 0.0009999999999999994
  (1 4): 0.0009999999999999994
  (2 3): 0.0009999999999999994
  (4 7): 0.0009999999999999994
  (0 4): 0.0009999999999999994
  (8 11): 0.0009999999999999994
  (0 9): 0.0009999999999999994
  (7 8 10): 0.0009999999999999994
  (3 7): 0.0009999999999999994
  (2 10): 0.0009999999999999994
  (6 11): 0.0009999999999999994
  (4 5): 0.0009999999999999994
  (2 5): 0.0009999999999999994
  (3 10): 0.0009999999999999994
  (0 6): 0.0009999999999999994
  (6 7 11): 0.0009999999999999994
  (3 6): 0.0009999999999999994
  (1 5): 0.0009999999999999994
  (1 8): 0.0009999999999999994
  (0 11): 0.0009999999999999994
  (7 9): 0.0009999999999999994

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;
; Generate 446 random integers representing the 12 months (here 0..11)
; and show the min, mean, and max values as well as the probability that
; the max value is 30% larger the mean value.
;
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 446) ; number of women
   (define m 12) ; months

   ; For each of the 446 women, generate a random birth month (0..11)
   (define birthMonths (for/list ([i n]) (random-integer 12)))

   ;; The number of births of each month
   (define births (for/list ([j m]) (count-occurrences j birthMonths)))
   
   (define minVal (apply min births))
   (define meanVal (avg births))
   (define maxVal (apply max births))
   
   ; What is the probability that the max value is larger than 30% of the mean value?
   (define p (> maxVal (* meanVal 1.3)))

   ; How many months has values larger than 30% of the mean value?
   (define p2 (for/sum ([j m])  (if (> (list-ref births j) (* meanVal 1.3)) 1 0)))

   ; All months that has > 30% of the mean value
   (define gt-months (for/list ([j m]
                                #:when (> (list-ref births j) (* meanVal 1.3)))
                       j))
    
   (list minVal
         meanVal
         maxVal
         p
         p2
         gt-months
         )
   )
)

(show-marginals (model)
                (list  "minVal"
                       "meanVal"
                       "maxVal"
                       "p"
                       "p2"
                       "gt-months"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.94
                #:show-histogram? #t
                #:show-percentiles? #t
                )


