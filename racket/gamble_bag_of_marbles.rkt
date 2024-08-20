#| 

  Bag of Marbles in Racket Gamble.

  "Probabilistic logic programming and its applications"
  Luc De Raedt, Leuven
  https://www.youtube.com/watch?v=3lnVBqxjC88
  @ 3:44

  """
  Mike has a bag of marbles with 4 white, 8 blue, and
  6 red marbles. He pulls out one marble from the bag
  and it is red. What is the probability that the
  second marble he pulls out of the bag is white?

  The answer is 0.235941
  """

  This is a port of my WebPPL model bag_of_marbles.wppl
  
  As with the WebPPL model (as well as the cplint and PSI models), the
  probabilitu of draw1 == white is a little different from the example: 
  0.23529411764705882 (vs 0.235941). Perhaps a typo?

  var : draw0
  red: 1 (1.0)

  var : draw1
  blue: 8/17 (0.47058823529411764)
  red: 5/17 (0.29411764705882354)
  white: 4/17 (0.23529411764705882)

  var : draw1 == white
  #f: 13/17 (0.7647058823529411)
  #t: 4/17 (0.23529411764705882)
  mean: 4/17 (0.23529411764705882)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (bag-of-marbles)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler ; slower than importance-sampler
   
   (define colors-list (list "white" "blue" "red"))
   (define colors (list->vector colors-list))
   
   (define (start c) (list-ref '(4 8 6) (index-of colors-list c)))

   ; First draw of a marble
   (define draw0 (categorical-vw colors (vector (start "white") (start "blue") (start "red"))))

   ; Second draw
   (define draw1
     (case draw0
       [("white") (categorical-vw colors (vector (sub1 (start "white")) (start "blue") (start "red")))]
       [("blue")  (categorical-vw colors (vector (start "white") (sub1 (start "blue")) (start "red")))]
       [("red")   (categorical-vw colors (vector (start "white") (start "blue") (sub1 (start "red"))))]))
      
   (observe/fail (eq? draw0 "red"))

   (list draw0 draw1 (eq? draw1 "white"))
   
   )
  )

(show-marginals (bag-of-marbles) '("draw0" "draw1" "draw1 == white"))
