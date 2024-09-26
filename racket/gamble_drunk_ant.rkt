#| 

  Drunk Ant in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/202
  """
  An ant is standing on one corner of a cube & can only walk on the 
  edges. The ant is drunk and from any corner, it moves randomly by 
  choosing any edge! What is the expected number of edges the ant travels, 
  to reach the opposite corner?

  Answer: 10
  """

  var : a
  (0 1 5 7): 0.03726000000002012
  (0 2 6 7): 0.03721000000002009
  (0 4 5 7): 0.036960000000019956
  (0 4 6 7): 0.03688000000001992
  (0 1 3 7): 0.036460000000019685
  ...
  (0 4 6 4 6 2 0 1 3 2 0 1 3 7): 1.00000000000054e-5
  (0 1 3 1 0 4 0 4 5 1 0 2 6 7): 1.00000000000054e-5
  (0 1 0 4 6 2 0 2 3 1 3 7): 1.00000000000054e-5
  (0 4 0 1 3 1 3 2 6 2 0 4 5 7): 1.00000000000054e-5
  (0 2 6 4 0 1 0 2 3 2 6 2 0 4 5 4 0 4 5 1 3 7): 1.00000000000054e-5

  var : len
  3: 0.22014000000011885
  5: 0.1732800000000935
  7: 0.1344200000000724
  9: 0.1063000000000572
  11: 0.0804000000000444
  ...
  71: 2.00000000000108e-5
  83: 2.00000000000108e-5
  81: 1.00000000000054e-5
  85: 1.00000000000054e-5
  93: 1.00000000000054e-5
  mean: 10.025880000005328


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


#|
      4--------5
    / |      / |
   0--------1  |
   |  |     |  |
   |  |     |  |
   |  6-----|--7
   | /      | /
   2--------3


   Edges: 0 -> 1 2 4
          1 -> 0 3 5
          2 -> 0 3 6
          3 -> 1 2 7
          4 -> 0 5 6
          5 -> 1 4 7
          6 -> 2 4 7
          7 -> 3 5 6
|#

(define edges '( (1 2 4)
                 (0 3 5)
                 (0 3 6)
                 (1 2 7)
                 (0 5 6)
                 (1 4 7)
                 (2 4 7)
                 (3 5 6)))

(define (model)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define start 0)
   (define end 7)

   (define (f a)
     (let ([pos (last a)])
       (if (= pos end)
           a
           (let ([new-pos (uniform-draw (list-ref edges pos))])
             (f (append a (list new-pos)))))))

   (define a (f (list start)))
   (define len (- (length a) 1)) ; Don't count the start node

   (list a
         len)

   )
)

(show-marginals (model)
                (list  "a"
                       "len"
                       )
                #:num-samples 100000
                #:truncate-output 5
                )


