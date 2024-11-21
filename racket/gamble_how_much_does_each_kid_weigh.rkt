#| 

  How much does each kid weigh? in Racket/Gamble 

  From MindYourDecisions 
  """
  Singapore nice homework puzzle

  Four children were experimenting with a scale. As each did not want the others 
  to know their individual mass, they agreed to weigh 3 people at a time. 
  After trying all possible combinations, they kept getting the same four 
  readings on the weighing scale.

  63 kg
  66 kg
  69 kg
  72 kg

  But then they realized the group weighings provided too much information! 
  What is the mass of each child?
  """

  variable : kids
  (27 24 21 18): 1 (1.0)

  This takes about 1 minute. 

  Compare with the Picat model http://hakank.org/picat/how_much_does_each_kid.weigh.pi
  which takes about 2ms.

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

   (define totals '(63 66 69 72))

   (define n 4)
   (define kids (for/list ([i n]) (add1 (random-integer 30))))

   ; Check all triplets 
   (for ([s (boolean-subsets 4 3)]
         [c (in-naturals)])
     (let* ([t (scalar-product kids s)])
       (observe/fail (= (list-ref totals c) t))
       ))

   (list kids
         )
     
   )
)

(show-marginals (model)
              (list  "kids"))


