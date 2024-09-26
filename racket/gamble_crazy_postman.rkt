#| 

  Crazy Postman in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/1003
  """
  A postman brought N letters to a house with two letter-boxes. Since the 
  two boxes were empty, he puts 1 mail in each of the two mail boxes. Then 
  he chooses one of boxes with probability proportional to number of letters 
  present in that box, and puts the 3rd letter in it. He does this for all 
  subsequent letters. What is the expected number of letters in the box with 
  lower letters?

  Solution: ... So, expected number of letters in the smaller box is 
            asymptotically n/4
  """

  * For two letter boxes, enumerate using model1

    num-letters = 10

    var : min-num-letters
    1: 2/11 (0.18181818181818182)
    2: 2/11 (0.18181818181818182)
    3: 2/11 (0.18181818181818182)
    4: 2/11 (0.18181818181818182)
    5: 2/11 (0.18181818181818182)
    6: 1/11 (0.09090909090909091)
    mean: 36/11 (3.272727272727273)

    num-letters = 20 

    var : min-num-letters
    1: 2/21 (0.09523809523809523)
    2: 2/21 (0.09523809523809523)
    3: 2/21 (0.09523809523809523)
    4: 2/21 (0.09523809523809523)
    5: 2/21 (0.09523809523809523)
    6: 2/21 (0.09523809523809523)
    7: 2/21 (0.09523809523809523)
    8: 2/21 (0.09523809523809523)
    9: 2/21 (0.09523809523809523)
    10: 2/21 (0.09523809523809523)
    11: 1/21 (0.047619047619047616)
    mean: 121/21 (5.761904761904762)

  * Model 2 is a generalization and supports arbitrary number of boxes.
    Importance-sampler is used since updates is used (which enumerate 
    does not handles well). Also, we add max-num-letters
    
    num-boxes=2, num-letters = 10 

    var : min-num-letters
    2: 0.18510000000000001
    4: 0.18420000000000003
    3: 0.18180000000000002
    1: 0.18000000000000002
    5: 0.17890000000000003
    6: 0.09000000000000001
    mean: 3.2669000000000006

    var : max-num-letters
    10: 0.18510000000000001
    8: 0.18420000000000003
    9: 0.18180000000000002
    11: 0.18000000000000002
    7: 0.17890000000000003
    6: 0.09000000000000001
    ...
    mean: 8.7331

   
    num-boxes = 2 num-letters = 100

    var : min-num-letters
    26: 0.0232
    8: 0.0227
    23: 0.0224
    36: 0.0221
    16: 0.0215
    ...
    19: 0.018
    11: 0.0177
    14: 0.0172
    5: 0.0168
    51: 0.009
    mean: 25.8806

    var : max-num-letters
    76: 0.0232
    94: 0.0227
    79: 0.0224
    66: 0.0221
    86: 0.0215
    ...
    83: 0.018
    91: 0.0177
    88: 0.0172
    97: 0.0168
    51: 0.009
    mean: 76.11939999999998

    num-boxes = 3 num-letters = 100

    var : min-num-letters
    1: 0.05500000000000012
    5: 0.05470000000000013
    4: 0.053700000000000116
    3: 0.0526000000000001
    2: 0.0524000000000001
    ...
    30: 0.007400000000000016
    31: 0.006700000000000015
    32: 0.004000000000000009
    33: 0.002800000000000006
    34: 0.0004000000000000009
    mean: 11.858300000000025

    var : max-num-letters
    51: 0.030500000000000072
    53: 0.030500000000000065
    52: 0.030000000000000065
    56: 0.029200000000000063
    54: 0.028100000000000055
    ...
    98: 0.0020000000000000044
    99: 0.0013000000000000028
    100: 0.0012000000000000025
    35: 0.0010000000000000022
    101: 0.0005000000000000011
    mean: 62.458700000000135


    num-boxes = 10 num-letters = 100
    var : min-num-letters
    1: 0.5937000000000007
    2: 0.2599000000000004
    3: 0.10030000000000017
    4: 0.03470000000000006
    5: 0.009400000000000014
    6: 0.0015000000000000026
    ...
    7: 0.0005000000000000009
    mean: 1.6127000000000025

    var : max-num-letters
    26: 0.0598000000000001
    29: 0.058300000000000095
    27: 0.056500000000000085
    28: 0.05360000000000009
    25: 0.05230000000000009
    ...
    69: 0.00030000000000000046
    67: 0.00020000000000000033
    68: 0.00020000000000000033
    71: 0.00010000000000000017
    75: 0.00010000000000000017
    mean: 31.150700000000054


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

   (define num-letters 10) ; number of letters

   (define (f i b1 b2)
     (if (>= i num-letters)
         (list b1 b2)
         (let* ([new-b (categorical-vw2 (vector b1 b2) (vector 1 2))]
                [new-b1 (if (= new-b 1) (add1 b1) b1)]
                [new-b2 (if (= new-b 2) (add1 b2) b2)])
           (f (add1 i) new-b1 new-b2))))
   
   (define res (f 0 1 1))
   (define a (first res))
   (define b (second res))

   (define min-num-letters (apply min res))

   (list ; res
         ; a
         ; b
         min-num-letters
    )
    
                      
   )
)

(displayln "Model 1")
(show-marginals (model)
                (list  ; "res"
                       ; "a"
                       ; "b"
                       "min-num-letters"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )



#|
  Generalized version to support arbitrary number of letter boxes.
  
  Note: Since updates are used enumerate give very strange results.

|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num-boxes 2)   
   (define num-letters 100) 

   (define boxes (ones-vector num-boxes 1))
   (define boxes-ids (list->vector (range num-boxes)))
   
   (define (f i)
     (if (>= i num-letters)
         boxes
         (let ([new-b (categorical-vw2 boxes boxes-ids)])
           (vector-set! boxes new-b (add1 (vector-ref boxes new-b)))
           (f (add1 i)))))

   (define res (vector->list (f 0)))
   (define min-num-letters (apply min res))
   (define max-num-letters (apply max res))   

   (list ; res
         min-num-letters
         max-num-letters
         )
    
                      
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  ; "res"
                       "min-num-letters"
                       "max-num-letters"                       
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


