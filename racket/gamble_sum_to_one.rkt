#| 

  Sum to one in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/213
  """
  On pressing a button, a random number is generated uniformly between 0 & 1. 
  You keep on generating these numbers until their sum exceeds 1. What is 
  the probability that you need to press the button more than n times? What 
  is the expected number of times you need to press the button?

  Answer: Probability: 1/n! ; Expected times: e
  """

  var : len
  2: 0.500294
  3: 0.332722
  4: 0.125544
  5: 0.033215
  6: 0.006772
  7: 0.001259
  8: 0.000167
  9: 2.6e-5
  11: 1e-6
  mean: 2.7180310000000003

  Cf https://math.stackexchange.com/questions/111314/choose-a-random-number-between-0-and-1-and-record-its-value-keep-doing-it-u/111343#111343

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

   (define (f a)
     (if (> (sum a) 1)
         a
         (f (append a (list (beta 1 1))))))

   (define a (f '()))
   (define len (length a))
   
   (list len)

   )
)

(show-marginals (model)
                (list  "len"
                       )
                #:num-samples 1000000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


