#| 

  Breaking Stick in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/119 
  """
  The stick drops and breaks at a random point distributed uniformly across 
  the length. What is the expected length of the smaller part?
  """

  * (beta 1 1): 0..1
  var : smallest
  0.3756024057169352: 1.0000000000019164e-5
  0.05246192212649059: 1.0000000000019164e-5
  0.04223474786674675: 1.0000000000019164e-5
  0.27157169417251903: 1.0000000000019164e-5
  0.06872360300537883: 1.0000000000019164e-5
  ...
  0.42561705484562107: 1.0000000000019164e-5
  0.07244369644359609: 1.0000000000019164e-5
  0.21620417284508597: 1.0000000000019164e-5
  0.4291495165496194: 1.0000000000019164e-5
  0.03688059451737613: 1.0000000000019164e-5
  mean: 0.2504999118502849

  * random-integer 100 enumerate
    shows the same behavior

  Model 2 1..100 
  var : smallest
  1: 1/50 (0.02)
  2: 1/50 (0.02)
  3: 1/50 (0.02)
  4: 1/50 (0.02)
  5: 1/50 (0.02)
  ...
  47: 1/50 (0.02)
  48: 1/50 (0.02)
  49: 1/50 (0.02)
  0: 1/100 (0.01)
  50: 1/100 (0.01)
  mean: 25 (25.0)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(displayln "Model 1 (beta 1 1)")
(define (model1)
  (; importance-sampler
   mh-sampler

   (define part1 (beta 1 1))
   (define part2 (- 1 part1))
   (define smallest (if (< part1 part2) part1 part2))

   (list smallest)

   )
)

(show-marginals (model1)
              (list  "smallest")
                    #:num-samples 100000
                    #:truncate-output 5)



(displayln "Model 2 1..100 ")
(define (model2)
  (enumerate
   
   (define n 100)
   (define part1 (add1 (random-integer n)))
   (define part2 (- n part1))

   (define smallest (if (< part1 part2) part1 part2))

   (list smallest)

   )
)

(show-marginals (model2)
              (list  "smallest"))


