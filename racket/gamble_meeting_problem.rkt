#| 

  Meeting problem in Racket.Gamble 

  (I'm not sure about the source. The page I got it from does not exist anymore.)
  """
  Let's say that 4 people agree to meet between 3:00 P.M. and 4:00 P.M.. 
  One man can wait for 10 minutes. Another can wait for 15 minutes. 
  Another can wait for 20 Minutes, and another still can wait for 30 
  minutes. What is the probability that all 4 will meet?
  """

  Note: Here we assume that the people might wait later than 4:00pm if needed.

  * If we allow meetings after 4:00pm:

    The probability that all meet is 0.069

  Using enumerate (it took 1min48s):

  var : prob
  #f: 402013/432000 (0.9305856481481481)
  #t: 29987/432000 (0.06941435185185185)
  mean: 29987/432000 (0.06941435185185185)

  var : size
  0: 402013/432000 (0.9305856481481481)
  1: 144839/12960000 (0.011175848765432098)
  2: 8729/864000 (0.01010300925925926)
  3: 23453/2592000 (0.009048225308641975)
  4: 103901/12960000 (0.008017052469135803)
  ...
  6: 78379/12960000 (0.006047762345679012)
  10: 181/34560 (0.005237268518518519)
  7: 13273/2592000 (0.005120756172839506)
  8: 407/96000 (0.004239583333333333)
  9: 44191/12960000 (0.003409799382716049)
  mean: 1359847/4320000 (0.31477939814814815)

  var : min_val
  0: 402013/432000 (0.9305856481481481)
  30: 127/80000 (0.0015875)
  31: 127/80000 (0.0015875)
  32: 127/80000 (0.0015875)
  33: 127/80000 (0.0015875)
  ...
  13: 2197/12960000 (0.0001695216049382716)
  12: 1/7500 (0.00013333333333333334)
  69: 77/720000 (0.00010694444444444445)
  11: 1331/12960000 (0.00010270061728395061)
  10: 1/12960 (7.716049382716049e-5)
  mean: 1171841/405000 (2.8934345679012345)

  * If we restrict to strictly between 3:00pm..4:00pm (i.e. no extra waiting) 
    then the probability is much slower: 1/216000 (4.6296296296296296e-6)
    Enumerate took about 47s,

  var : prob
  #f: 215999/216000 (0.9999953703703703)
  #t: 1/216000 (4.6296296296296296e-6)
  mean: 1/216000 (4.6296296296296296e-6)

  var : size
  0: 215999/216000 (0.9999953703703703)
  1: 1/216000 (4.6296296296296296e-6)
  mean: 1/216000 (4.6296296296296296e-6)

  var : min_val
  0: 215999/216000 (0.9999953703703703)
  1: 1/12960000 (7.71604938271605e-8)
  2: 1/12960000 (7.71604938271605e-8)
  ...
  59: 1/12960000 (7.71604938271605e-8)
  60: 1/12960000 (7.71604938271605e-8)
  mean: 61/432000 (0.00014120370370370372)

  Cf gamble_meeting_problem2.rkt which models this restricted version of the
  problem.

  This problem was originally done using R (see http://www.hakank.org/sims/simulering.html)
  which gave the probability of meeting: 0.073.

  This is a port of my WebPPL model meeting_problem.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(require racket/set) ; for set-intersect

(define (model)
  (enumerate ; #:limit 1e-05
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define w '(10 15 20 30))
   (define n (length w))

   ;; Start times + the wait times
   (define ps (for/list ([i n])
                (let ([r (add1 (random-integer 60))])
                  ; (range r (+ r (list-ref w i)))
                  (range r (min (+ r (list-ref w i))) 60)
                  )))
  
   ;; Is there a common time slot?
   ;; Intersection of the time ranges
   (define common (apply set-intersect ps))
    
   (define size (length common))
   (define prob (> size 0)) 
   (define min_val (if (> size 0) (first common) 0))
   
   (list prob      
         size
         min_val
         )
   
   )
)

(show-marginals (model)
              (list  "prob"
                     "size"
                     "min_val"
                     )
                    #:num-samples 100000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


