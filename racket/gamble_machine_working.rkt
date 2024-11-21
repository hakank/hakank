#| 

  Machine working in Racket/Gamble 

  https://pedrozudo.github.io/docs/20_thesis/thesis_final_online_compressed.pdf
  """
  Example 1.1. We model two machines (Line 1 in the program below). We want to know
  the probability of the first machine working (Line 11), given that the second machine
  works (Line 10) and given a model that describes under which conditions the machines
  work (Lines 7 and 8). Additionally the program models the outside temperature (Line
  3) and whether the cooling of each machine works (Lines 4 and 5) as (Boolean) random
  variables (expressed as probabilistic facts).
    1 machine(1). machine(2).
    2
    3 0.8::temperature(low).
    4 0.99::cooling(1).
    5 0.95::cooling(2).
    6
    7 works(N):- machine(N), cooling(N).
    8 works(N):- machine(N), temperature(low).
    9
    10 evidence(works(2)).
    11 query(works(1)).
  Running the program yields p(works(1)|works(2)) ≈ 0.998
  """

  variable : temp_low
  #t: 80/99 (0.8080808080808081)
  #f: 19/99 (0.1919191919191919)
  mean: 80/99 (0.8080808080808081)
  
  variable : cooling 0
  #t: 99/100 (0.99)
  #f: 1/100 (0.01)
  mean: 99/100 (0.99)

  variable : cooling 1
  #t: 95/99 (0.9595959595959596)
  #f: 4/99 (0.04040404040404041)
  mean: 95/99 (0.9595959595959596)
  
  variable : works 0
  #t: 9881/9900 (0.9980808080808081)
  #f: 19/9900 (0.0019191919191919192)
  mean: 9881/9900 (0.9980808080808081)

  variable : works 1
  #t: 1 (1.0)
  mean: 1 (1.0)

  variable : both_works
  #t: 9881/9900 (0.9980808080808081)
  #f: 19/9900 (0.0019191919191919192)
  mean: 9881/9900 (0.9980808080808081)


  This is a port of my WebPPL model machine_working.wppl

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

    (define temp_low (flip 8/10))
    (define cooling (list (flip 99/100) (flip 95/100)))
    (define (works m) 
      (or (list-ref cooling m) temp_low))

    ; Machine 2 works
    (observe/fail (works 1))

    (define both_works (and (works 0) (works 1)))
    
    (list temp_low
          (list-ref cooling 0)
          (list-ref cooling 1)
          (works 0)
          (works 1)
          both_works
    )
   )
)

(show-marginals (model)
                (list  "temp_low"
                       "cooling 0"
                       "cooling 1"
                       "works 0"
                       "works 1"
                       "both_works"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


