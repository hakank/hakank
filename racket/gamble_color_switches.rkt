#| 

  Color switches in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/1020
  """
  You are given an urn with 100 balls (50 black and 50 white). You pick 
  balls from urn one by one without replacements until all the balls are 
  out. A black followed by a white or a white followed by a black is 
  "a colour change". Calculate the expected number of colour-changes if 
  the balls are being picked randomly from the urn.

  Answer: 50
  """

  * For 10 balls (5 black and 5 white) using enumerate
 
  var : num-color-switches
  5: 2/7 (0.2857142857142857)
  4: 4/21 (0.19047619047619047)
  6: 4/21 (0.19047619047619047)
  3: 8/63 (0.12698412698412698)
  7: 8/63 (0.12698412698412698)
  2: 2/63 (0.031746031746031744)
  8: 2/63 (0.031746031746031744)
  1: 1/126 (0.007936507936507936)
  9: 1/126 (0.007936507936507936)
  mean: 5 (5.0)

  * For n = 10 (10 black and 10 white)

  var : num-color-switches
  9: 7938/46189 (0.17185910065167032)
  10: 7938/46189 (0.17185910065167032)
  11: 7938/46189 (0.17185910065167032)
  8: 5292/46189 (0.11457273376778021)
  12: 5292/46189 (0.11457273376778021)
  7: 3528/46189 (0.07638182251185348)
  13: 3528/46189 (0.07638182251185348)
  6: 1512/46189 (0.03273506679079435)
  14: 1512/46189 (0.03273506679079435)
  5: 648/46189 (0.014029314338911861)
  15: 648/46189 (0.014029314338911861)
  4: 162/46189 (0.0035073285847279654)
  16: 162/46189 (0.0035073285847279654)
  3: 81/92378 (0.0008768321461819913)
  17: 81/92378 (0.0008768321461819913)
  2: 9/92378 (9.742579402022126e-5)
  18: 9/92378 (9.742579402022126e-5)
  1: 1/92378 (1.082508822446903e-5)
  19: 1/92378 (1.082508822446903e-5)
  mean: 10 (10.0)

  * For 50 black and 50 white

  var : num-color-switches
  50: 0.0842
  51: 0.0792
  49: 0.0778
  52: 0.0727
  48: 0.0721
  47: 0.068
  ...
  34: 0.0006
  67: 0.0003
  33: 0.0003
  66: 0.0001
  68: 0.0001
  mean: 50.056800000000024



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

   (define n 50)
   (define balls (append (ones-list n "black") (ones-list n "white")))
   
   (define balls-shuffled (shuffle balls) ) ; for importance-sampler
   ; (define balls-shuffled (draw-without-replacement (length balls) balls) ) ; for enumerate

   
   
   (define num-color-switches
     (for/sum ([i (range 1 (length balls-shuffled))])
       (if (not (eq? (list-ref balls-shuffled (sub1 i)) (list-ref balls-shuffled i))) 1 0)))
   
   (list ; balls
         num-color-switches
         )
   

   )
)

(show-marginals (model)
                (list  ; "balls"
                       "num-color-switches"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


