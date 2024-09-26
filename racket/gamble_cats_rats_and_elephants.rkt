#| 

  Cats, rats, and elephants in Racket Gamble.

  "Cats and rats and elephants"
  https://www.allendowney.com/blog/2018/12/11/cats-and-rats-and-elephants/
  """
  A few weeks ago I posted 'Lions and Tigers and Bears', 
  (https://www.allendowney.com/blog/2018/12/03/lions-and-tigers-and-bears/)
  which poses a Bayesian problem related to the Dirichlet distribution.  If you have 
  not read it, you might want to start there.

  Now here’s the follow-up question:

    Suppose there are six species that might be in a zoo: lions and tigers and bears, 
    and cats and rats and elephants. Every zoo has a subset of these species, and every 
    subset is equally likely.

    One day we visit a zoo and see 3 lions, 2 tigers, and one bear. Assuming that every 
    animal in the zoo has an equal chance to be seen, what is the probability that the 
    next animal we see is an elephant?
  """

  Note: The approach in this model is the same as in the
  "Lions and Tigers and Bears" problem, just added
    - the three new animals
    - queries which calculates subsets and number of different animals

  With observe/fail this is extremely slow. Using observe-sample and normal-dist is much 
  faster and also fairly accurate. Here's the output for 10000 sample (importance-sampler):

var : o 0
lion: 0.263529914337238
tiger: 0.2524406290252267
bear: 0.20744821205827224
cat: 0.13275210888834968
rat: 0.10066945826283195
elephant: 0.043159677428077844
...

var : o 0 == elephant
#f: 0.9568403225719219
#t: 0.043159677428077844
mean: 0.043159677428077844

var : a6
(lion tiger bear): 0.20981508924082373
(tiger lion bear): 0.16018793564097972
(lion tiger): 0.12485253078753611
(tiger lion): 0.07302108133132981
(lion bear tiger): 0.06537627950434602
...
(elephant rat bear): 1.1965433482289625e-17
(rat elephant cat lion): 1.1965433482289624e-17
(elephant rat cat lion): 3.613247826789475e-19
(rat elephant lion): 1.8182395093758688e-19
(elephant rat cat bear): 4.8899991804262886e-20

var : numAnimals
3: 0.6273366652830447
2: 0.2508266088591101
4: 0.11854464501023643
1: 0.002689102733821751
5: 0.0006029640450125708
6: 1.406877132630337e-8
...
mean: 2.8635458009798125


  This is a port of my WebPPL cats_rats_and_elephants.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler ; #:transition (enumerative-gibbs)
   
   ;; The animals.
   (define lion     0)
   (define tiger    1)
   (define bear     2)
   (define cat      3)
   (define rat      4)
   (define elephant 5)
    
   ;; Prior
   ;; We draw 6 times with the multinomial distribution.
   ;; What is the probability of different combinations of the number of each animal?
   ; (define alphas (vector 1/6 1/6 1/6 1/6 1/6 1/6))
   (define alphas (vector 60 50 40 30 20 10))
    
   ;; Draw 6 animals
   (define x (dirichlet alphas))
   
   ; (define x (for/vector ([i 6]) (multinomial2 (list->vector (range 6)) alphas)))
   ; (define x (sample (multinomial-dist 6 alphas)))
   ; (show2 "x" x "sum x" (sum (vector->list x)))
   ; (observe/fail (> (sum (vector->list x)) 0))
   ; (define x (for/vector ([i 6]) (categorical-vw2 (list->vector (range 6)) (vector 1 1 1 1 1 1))))

   ;; The probabilities to calculate ("aliased" for simplicity)
   (define probLion     (vector-ref x 0))
   (define probTiger    (vector-ref x 1))
   (define probBear     (vector-ref x 2))
   (define probCat      (vector-ref x 3))
   (define probRat      (vector-ref x 4))
   (define probElephant (vector-ref x 5))
    
   ;; Posterior: What is the probability of lion, tiger, and bear given the observations?
   (defmem (o i)
     (categorical-vw2 x
                      (vector lion tiger bear cat rat elephant)))
    
   ;; Dictionary, for presentation of the subsets
   (define a '("lion" "tiger" "bear" "cat" "rat" "elephant"))
   
   ;; Observations
   ;; This is too slow!
   ;; (observe/fail (= (o 0) lion))
   ;; (observe/fail (= (o 1) lion))
   ;; (observe/fail (= (o 2) lion))
   ;; (observe/fail (= (o 3) tiger))
   ;; (observe/fail (= (o 4) tiger))
   ;; (observe/fail (= (o 5) bear))

   ; Faster, but we have to tweak the precision to get a
   ; fairly correct result.
   (define sigma 1)
   (observe-sample (normal-dist (o 0) sigma) lion)
   (observe-sample (normal-dist (o 1) sigma) lion)
   (observe-sample (normal-dist (o 2) sigma) lion)
   (observe-sample (normal-dist (o 3) sigma) tiger)
   (observe-sample (normal-dist (o 4) sigma) tiger)
   (observe-sample (normal-dist (o 5) sigma) bear)
   
   
   ;; Which subset of the animals is the most probable?
   (define a6 (remove-duplicates (for/list ([i 6]) (list-ref a (o i)))))
    
   ;; Number of different animals in the zoo
   (define numAnimals (length a6))
  
   (list
        (list-ref a (o 6))
        (= (o 6) elephant)
        a6
        numAnimals
    )

   )
  )

(show-marginals (model)
                (list "o 0"
                      "o 0 == elephant"
                      "a6"
                      "numAnimals"
                      )

                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                
                )

