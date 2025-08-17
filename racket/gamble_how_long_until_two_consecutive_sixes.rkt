#| 

  How long until you roll two consecutive sixes in Racket/Gamble 

  Dr. Robert Kübler "How Long Until You Roll Two Consecutive Sixes?"
  https://www.cantorsparadise.com/how-long-until-you-roll-two-consecutive-sixes-e184292141d5
  """
  You roll a fair standard die repeatedly, until you get two consecutive sixes. 
  What is the expected number of rolls before the game ends?
  """

  variable : len
  2: 0.02690000000000005
  3: 0.026500000000000048
  4: 0.02250000000000004
  10: 0.021500000000000036
  13: 0.021300000000000038
  ...
  236: 0.00010000000000000018
  241: 0.00010000000000000018
  242: 0.00010000000000000018
  250: 0.00010000000000000018
  252: 0.00010000000000000018
  mean: 41.70700000000008
  Min: 2 Mean: 42.3818 Max: 398 Variance: 1639.93442876 Stddev: 40.49610387136027
  HPD interval (0.5): 2..30
  HPD interval (0.84): 2..76
  HPD interval (0.9): 2..96
  HPD interval (0.95): 2..125
  HPD interval (0.99): 2..187

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
     (let ([t (take-last a 2)])
       (if (equal? t '(6 6))
           a
           (let ([d (add1 (random-integer 6))])
             (f (append a (list d)))
       )
     )))

   (define a (f '()))
   (define len (length a))

   (list len)

   )
)

(show-marginals (model)
              (list  "len"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.5 0.84 0.9 0.95 0.99)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


