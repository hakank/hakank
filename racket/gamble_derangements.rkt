#| 

  Derangements in Racket Gamble.

  From http://rosettacode.org/wiki/Permutations/Derangements
  """
  Permutations/Derangements
  A derangement is a permutation of the order of distinct items in which 
  no item appears in its original place.

  For example, the only two derangements of the three items 
    (0, 1, 2) are (1, 2, 0), and (2, 0, 1).
  
  The number of derangements of n distinct items is known as the subfactorial of n, 
  sometimes written as !n. There are various ways to calculate !n.
  ...
  """

  Two variables:
  - total: the number of i in i'th position.
    The expectation of total is 1
  - totalIs0: probability that total is 0, i.e. a derangement

  * For n=8 enumerate (1 min 27s)

   var : total
   0: 2119/5760 (0.36788194444444444)
   1: 103/280 (0.3678571428571429)
   2: 53/288 (0.1840277777777778)
   3: 11/180 (0.06111111111111111)
   4: 1/64 (0.015625)
   5: 1/360 (0.002777777777777778)
   6: 1/1440 (0.0006944444444444445)
   8: 1/40320 (2.48015873015873e-5)
   mean: 1 (1.0)

   var : total = 0
   #f: 3641/5760 (0.6321180555555556)
   #t: 2119/5760 (0.36788194444444444)
   mean: 2119/5760 (0.36788194444444444)

  * n=8 importance-sampler (16s)

   var : total
   1: 0.36700000000000005
   0: 0.36300000000000004
   2: 0.19000000000000003
   3: 0.061000000000000006
   4: 0.015000000000000001
   5: 0.0020000000000000005
   6: 0.0020000000000000005
   mean: 1.0120000000000002

   var : total = 0
   #f: 0.6370000000000001
   #t: 0.36300000000000004
   mean: 0.36300000000000004


  This is a port of my WebPPL model derangements.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (derangements n)

  (; enumerate ; slow
   rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define p (mem (lambda (i) 
        (random-integer n))))

   (define seq (for/list ([i (range n)]) (p i)))

   ;; Ensure unicity.
   ;; Using _.uniq is faster (as well as placing the (observe/fail  early)
   (observe/fail (eq? (length seq) (length (remove-duplicates seq))))

   ;; How many are in the i'th position
   (define total (for/sum ([i (range n)]) (if (= i (p i)) 1 0)))
    
   ;; (observe/fail (= total 0))
    
   (list total
         (= total 0)
    )

   )
  )

(show-marginals (derangements 8)
                  (list ; "seq"
                        "total"
                        "total = 0"
                        )
                  #:num-samples 1000
                  ; #:truncate-output 1
                  ; #:skip-marginals? #t
                  )
