#| 

  Bertrand's paradox (boxes) in Racket.Gamble 

  From Julian Simon "Resampling Statistics", page 80ff
  """
  A Spanish treasure fleet of three ships was sunk at sea
  off Mexico. One ship had a trunk of gold forward and
  another aft, another ship had a trunk of gold forward
  and a trunk of silver aft, while a third ship had a trunk
  of silver forward and another trunk of silver aft. Divers
  just found one of the ships and a trunk of silver in it.
  They are now taking bets about whether the other trunk
  found on the same ship will contain silver or gold. What
  are fair odds?

  (This is a restatement of a problem that Joseph Bertrand posed
  early in the 19th century.) In the Goldberg variation:

  Three identical boxes each contain two coins. In one
  box both are pennies, in the second both are nickels,
  and in the third there is one penny and one nickel.
  A man chooses a box at random and takes out a coin.
  If the coin is a penny, what is the probability that the
  other coin in the box is also a penny?
  """

  Here are two different approaches:
  - model 1: resampling approach
  - model 2: "pure" PPL


  Cf gamble_bertrands_paradox.rkt for another approach using "pure" PPL.
  IMHO, model 2 below is a little neater.

  This is a port of my WebPPL model bertrands_paradox_resampling.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

#|
  Model 1: Using global updates

  (scores_len: 5088 scores_sum: 3395 z: 0.6672562893081762)

  Note: This does not work with Enumerate
|#
(displayln "Model 1: Using global updates:")
(define scores '())
(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define gg '("g" "g")) ; gold, gold
   (define gs '("g" "s")) ; gold, silver
   (define ss '("s" "s")) ; silver, silver

   (define ships (list gg gs ss))

   ;; Pick a ship
   (define ship (random-integer 3))

   ;; If we pick ship 0: then the other must be gold
   ;; If we pick ship 1: then we pick gold and thus then the other must be silver
   ;; If we pick ship 2: then it's not interesting (both are silver)
   (if (= ship 0) 
       ;; We pick ship 0: both are gold
       (set! scores (append scores (list 1)))
       (when (and (= ship 1) (eq? (uniform-draw (list-ref ships ship)) "g"))
           ;; We pick ship 1 and picked gold -> the other is silver: no hit
           (set! scores (append scores (list 0)))))

   (list ship
         )

   )
)

(show-marginals (model1)
              (list  "ship"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    )


(define scores_len (length scores))
(define scores_sum (sum scores))
(show2 "scores_len:" scores_len  "scores_sum:" scores_sum "z:" (* 1.0 (/ scores_sum scores_len)))

#|
  Model 2: Using enumerate

  var : ship
  0: 2/3 (0.6666666666666666)
  1: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)

  var : z
  #t: 2/3 (0.6666666666666666)
  #f: 1/3 (0.3333333333333333)
  mean: 2/3 (0.6666666666666666)


|#
(displayln "\nModel 2: Using Enumerate:")
(define (model2)
  (enumerate
   
   (define gg '("g" "g")) ; gold, gold
   (define gs '("g" "s")) ; gold, silver
   (define ss '("s" "s")) ; silver, silver

   (define ships (list gg gs ss))
   (define ship (random-integer 3))
   
   (define pick_side (random-integer 2))         ; Pick a random side
   (define other_side (if (= pick_side 0) 1  0)) ; Check the other side
   ; Did the other side has gold?
   (define z (eq? (list-ref (list-ref ships ship) other_side) "g"))

   ; We did found gold in the first pick   
   (observe/fail (eq? (list-ref (list-ref ships ship) pick_side) "g")) 
    
   (list ship
         z
    )
    
   )
)

(show-marginals (model2)
                (list  "ship"
                       "z"
                       )
                #:num-samples 10000
                #:truncate-output 5
                )

