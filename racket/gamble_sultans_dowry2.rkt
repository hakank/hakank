#| 

  Sultan's dowry in Racket/Gamble 

  https://mathworld.wolfram.com/SultansDowryProblem.html
  """
  A sultan has granted a commoner a chance to marry one of his n daughters. The 
  commoner will be presented with the daughters one at a time and, when each 
  daughter is presented, the commoner will be told the daughter's dowry (which 
  is fixed in advance). Upon being presented with a daughter, the commoner must 
  immediately decide whether to accept or reject her (he is not allowed to 
  return to a previously rejected daughter). However, the sultan will allow the 
  marriage to take place only if the commoner picks the daughter with the 
  overall highest dowry. Then what is the commoner's best strategy, assuming he 
  knows nothing about the distribution of dowries (B. Elbows)?

  Since the commoner knows nothing about the distribution of the dowries, the 
  best strategy is to wait until a certain number x of daughters have been 
  presented, then pick the highest dowry thereafter. The exact number to skip is
  determined by the condition that the odds that the highest dowry has already 
  been seen is just greater than the odds that it remains to be seen and that if 
  it is seen it will be picked. 
  ...
  The problem is most commonly stated with n = 100 daughters, which gives the 
  result that the commoner should wait until he has seen 37 of the daughters, 
  then pick the first daughter with a dowry that is bigger than any preceding 
  one. With this strategy, his odds of choosing the daughter with the highest 
  dowry are surprisingly high: about 37% 
  (B. Elbows; Honsberger 1979, pp. 104-110, Mosteller 1987).
  """
   
  Also see https://en.wikipedia.org/wiki/Secretary_problem 

  The theoretical solution is 1/exp(1) ~ 37% (~0.36787944117144233).

  
  * Using random-sample (from racket/random). This takes about 6s and gives
    a little larger value than the theoretical value:

  var : skip_pos
  37: 0.022999999999999986
  22: 0.021999999999999985
  23: 0.021999999999999985
  21: 0.019999999999999987
  43: 0.019999999999999987
  ...
  91: 0.0019999999999999987
  97: 0.0019999999999999987
  86: 0.0009999999999999994
  92: 0.0009999999999999994
  94: 0.0009999999999999994
  mean: 38.833999999999975
  HPD interval (0.84): 2..63


  * Using (shuffle r) for s is faster (about 2s) but it gives a slightly larger 
    value for skip_pct: about 43:
  var : skip_pos
  30: 0.023999999999999983
  21: 0.019999999999999987
  31: 0.019999999999999987
  35: 0.019999999999999987
  15: 0.018999999999999986
  ...
  89: 0.0019999999999999987
  92: 0.0019999999999999987
  95: 0.0019999999999999987
  96: 0.0009999999999999994
  97: 0.0009999999999999994
  mean: 43.77399999999995
  HPD interval (0.84): 5..71

  * Using enumerate #:limit 1e-15 is even faster (1.3s) but the values differs
    more wildly from 34 .. 44.


  This is a port of my WebPPL model sultans_dowry2.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(require racket/random)

(define n 100)
(define r (map add1 (range n)))

(define (model)
  (; enumerate #:limit 1e-15
   ; rejection-sampler
   ; importance-sampler
   mh-sampler
      
   ; The position to skip over
   (define skip_pos (random-integer n))
     
   ; Each princess has some random dowry (1..n)
   ; (define s (resample n r)) ; very slow
   (define s (random-sample r n)) ; faster than resample: 6s
   ; this is much faster (2s) but gives a slighly larger for skip_pos: about 43.  
   ; (define s (shuffle r)) 
   
   (define max_val (apply max s))

   ; Best value before the skip position
   (define best_before_skip
     (if (= skip_pos 0)
         0
         (apply max (for/list ((i skip_pos)) (list-ref s i))))
     )
   
   ; The values after skip position that are larger than the best value before skip position
   (define ds (for/first ((i (range skip_pos n))
                          #:do ((define si (list-ref s i)))
                          #:when (and (> i skip_pos) (> si best_before_skip)
                                     si
                                     0))
                si))

   ; Find the first value > best before skip position
   (define best_after_skip (if ds ds 0))
   
   ;; Did we get the best dowry?
   (define p (= best_after_skip max_val))
   
   (observe/fail p)
   
   (list skip_pos
         (* 1.0 (/ skip_pos n))
         p
         )
   )
)

(show-marginals (model)
                (list "skip_pos"
                      "skip_pct"
                       "p"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )

(displayln (format "Theoretical value: ~a" (/ 1 (exp 1))))
(newline)
