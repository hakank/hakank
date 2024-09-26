#| 

  Colored runs of cards in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/201
  """
  There are 26 black(B) and 26 red(R) cards in a standard deck. A run 
  is number of blocks of consecutive cards of the same color. For example, 
  a sequence RRRRBBBRBRB of only 11 cards has 6 runs; namely, 
  RRRR, BBB, R, B, R, B. Find the expected number of runs in a 
  shuffled deck of cards.
  """

  * importance-sampler

  var : rs
  28: 0.10907
  27: 0.10899
  26: 0.10745
  29: 0.09374
  25: 0.09294
  24: 0.08145
  30: 0.08127
  23: 0.05989
  31: 0.05801
  32: 0.04424
  22: 0.0418
  21: 0.02699
  33: 0.02623
  20: 0.01721
  34: 0.01649
  19: 0.0089
  35: 0.00865
  36: 0.00496
  18: 0.00443
  37: 0.00226
  17: 0.00213
  16: 0.00106
  38: 0.00089
  39: 0.00038
  15: 0.00026
  40: 0.00013
  41: 7e-5
  14: 6e-5
  42: 3e-5
  13: 2e-5
  mean: 27.003460000000004


  * Enumerate for 10 black and 10 red cards
    (use draw-without-replacement for this)
  var : rs
  10: 7938/46189 (0.17185910065167032)
  11: 7938/46189 (0.17185910065167032)
  12: 7938/46189 (0.17185910065167032)
  9: 5292/46189 (0.11457273376778021)
  13: 5292/46189 (0.11457273376778021)
  8: 3528/46189 (0.07638182251185348)
  14: 3528/46189 (0.07638182251185348)
  7: 1512/46189 (0.03273506679079435)
  15: 1512/46189 (0.03273506679079435)
  6: 648/46189 (0.014029314338911861)
  16: 648/46189 (0.014029314338911861)
  5: 162/46189 (0.0035073285847279654)
  17: 162/46189 (0.0035073285847279654)
  4: 81/92378 (0.0008768321461819913)
  18: 81/92378 (0.0008768321461819913)
  3: 9/92378 (9.742579402022126e-5)
  19: 9/92378 (9.742579402022126e-5)
  2: 1/92378 (1.082508822446903e-5)
  20: 1/92378 (1.082508822446903e-5)
  mean: 11 (11.0)



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

   (define n 26)

   ; This is faster than draw-without-replacement
   ; but enumerate just show an single mean (might might be incorrect)
   ; and without an distribution
   ; Use this for larger n (such as 26) + importance-sampler
   (define cards (shuffle (append (ones-list n "R") (ones-list n "B"))))

   ; This is slower but works well with  enumerate
   ; (define cards (draw-without-replacement (* n 2) (append (ones-list n "R") (ones-list n "B"))))

   
   (define rs (num-runs cards))

   (list rs
         )
   

   )
)

(show-marginals (model)
                (list  "rs"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


