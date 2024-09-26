#| 

  Distinct number draws in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/208
  """
  Given the set of numbers from 1 to n: {1, 2, 3 ... n}. We draw n 
  numbers randomly (with uniform distribution) from this set (with replacement). 
  What is the expected number of distinct values that we would draw?

  Answer: E(n) = n[1-(1-1/n)^n]
  """

  * enumerate n=6
  var : num-distinct
  4: 325/648 (0.5015432098765432)
  3: 25/108 (0.23148148148148148)
  5: 25/108 (0.23148148148148148)
  2: 155/7776 (0.01993312757201646)
  6: 5/324 (0.015432098765432098)
  1: 1/7776 (0.0001286008230452675)
  mean: 31031/7776 (3.9906121399176953)

  var : theoretical
  3.9906121399176953: 1 (1.0)
  mean: 3.9906121399176953

  * enumerate n=7

  var : num-distinct
  5: 7200/16807 (0.42839293151663)
  4: 6000/16807 (0.35699410959719163)
  6: 2160/16807 (0.128517879454989)
  3: 1290/16807 (0.07675373356339621)
  7: 720/117649 (0.006119899021666143)
  2: 54/16807 (0.003212946986374725)
  1: 1/117649 (8.499859752314087e-6)
  mean: 543607/117649 (4.620583260376204)

  var : theoretical
  4.620583260376204: 1 (1.0)
  mean: 4.620583260376204


  * n=10 (importance-sampler)
  var : num-distinct
  7: 0.35625999999999997
  6: 0.34847999999999996
  8: 0.13511999999999996
  5: 0.12604999999999997
  4: 0.017219999999999996
  9: 0.015969999999999998
  3: 0.0005999999999999998
  10: 0.0002999999999999999
  mean: 6.513319999999999


  * n=100

  var : num-distinct
  64: 0.146
  63: 0.119
  62: 0.109
  65: 0.107
  66: 0.097
  61: 0.088
  67: 0.07
  60: 0.066
  59: 0.049
  68: 0.045
  69: 0.032
  58: 0.025
  70: 0.015
  57: 0.012
  71: 0.01
  56: 0.004
  55: 0.003
  72: 0.002
  54: 0.001
  mean: 63.614

  var : theoretical
  63.39676587267705: 1.0
  mean: 63.39676587267705


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

; E(n) = n[1-(1-1/n)^n]
(define (theoretical n)
  (* n (- 1 (expt (- 1 (/ 1 n)) n)))
  )

(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 6)

   (define draws (for/list ([i n]) (random-integer n)))
   (define num-distinct (length (remove-duplicates draws)))

   (list num-distinct
         (* 1.0 (theoretical n))
    )

   )
)

(show-marginals (model)
                (list "num-distinct"
                      "theoretical"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


