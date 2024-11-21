#| 

  Zipf dist (recover parameters) in Racket/Gamble 

  Recover parameters for zipf 15 0.3

  Not too bad:

  data: (6 6 5 5 8 1 6 2 6 1 2 1 1 5 5 14 10 1 2 2)
  (min 1 mean 4.45 max 14)
  theoretical mean: 3.5773795828075943

  variable : n
  14: 0.1969000000000003
  16: 0.1791000000000002
  15: 0.1480000000000002
  17: 0.08100000000000011
  19: 0.06770000000000008
  ...
  13: 0.01570000000000002
  27: 0.013200000000000019
  26: 0.013000000000000018
  8: 0.00700000000000001
  12: 0.004300000000000006
  mean: 17.63710000000003

  variable : s
  0.10851066386565057: 0.11830000000000016
  0.06714669381419949: 0.11450000000000014
  0.44511115471439444: 0.11240000000000014
  0.21688583193166489: 0.0739000000000001
  0.14487007403117033: 0.06740000000000008
  ...
  4.940236438431121: 0.001400000000000002
  2.981171065495252: 0.001300000000000002
  8.906165108196983: 0.0010000000000000015
  5.676601338370954: 0.0010000000000000015
  3.18857427528674: 0.0006000000000000008
  mean: 0.39112732604879097

  variable : post
  1: 0.40350000000000075
  2: 0.19510000000000027
  4: 0.0738000000000001
  3: 0.061600000000000085
  5: 0.04790000000000008
  ...
  15: 0.003900000000000005
  19: 0.0018000000000000026
  24: 0.0010000000000000015
  25: 0.0004000000000000006
  18: 0.0003000000000000004
  mean: 3.832300000000006


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define data (for/list ([i 20]) (zipf 15 0.3)))
(show "data" data)
(show2 "min" (min-list data) "mean" (* 1.0 (avg data)) "max" (max-list data))
(show "theoretical mean" (zipf_mean 15 0.3))
(newline)

(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define n (add1 (random-integer (* 2 (max-list data)))))
   (define s (uniform 0 10))

   (for ([i (length data)])
     (observe-sample (normal-dist (zipf n s) 1) (list-ref data i)))

   (define post (zipf n s))

   (list n
         s
         post)

   )
)

(show-marginals (model)
                (list  "n"
                       "s"
                       "post"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84 0.9 0.95 0.99)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    #:burn 1000
                    #:thin 10
                    )


