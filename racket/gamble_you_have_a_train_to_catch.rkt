#| 

  You have a train to catch in Racket.Gamble 

  From https://brainstellar.com/puzzles/probability/20
  """
  Spiderman has two close friends, Mary Jane & Gwen Stacy. After every 
  mission, he rushes to the central subway. One line heads towards Mary's place, 
  and another towards Stacy. Trains from each line leave every 10 minutes. 
  Spiderman being impartial always boards the first train that leaves.

  However, he observes that he ends up visiting Mary Jane nine times more 
  often than Gwen Stacy. Can you decipher why?
  """

  This is a simple model for the problem. The unknown variable p is the
  percentages of trains towards Mary:

  var : p
  mean: 0.8344234822112969

  var : superman
  8: 0.10740000000000739
  3: 0.10330000000000727
  9: 0.10230000000000725
  2: 0.10180000000000723
  7: 0.1009000000000072
  5: 0.1008000000000072
  4: 0.10020000000000719
  0: 0.0973000000000071
  1: 0.093700000000007
  6: 0.09230000000000696
  mean: 4.552000000000325

  var : s
  M: 0.902599999999957
  G: 0.0974000000000071

  There is about 9 times more trains towards Mary than towards Gwen.

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

   ; Number of trains
   (define n 10)

   ; Probability of a train towards Mary
   (define p (beta 1 1))

   (define trains (for/list ([i n]) (if (flip p) "M" "G")))

   ; When did Superman arrive to the station
   (define superman (random-integer n))

   ; What train did he take? Towards Mary or towards Gwen?
   (define s (list-ref trains superman))

   ; Count the number of trains to Mary and Gwen
   (define count-mary (count-occurrences-eq "M" trains))
   (define count-gwen (count-occurrences-eq "G" trains))

   ; We observe 9 train towards Mary and 1 towards Gwen
   (observe-sample (dist-unit count-mary) 9)
   (observe-sample (dist-unit count-gwen) 1)   
   
   (list p
         ; trains
         superman
         s)
   
   )
)

(show-marginals (model)
                (list  "p"
                       ; "trains"
                       "superman"
                       "s"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


