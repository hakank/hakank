#| 

  Trip example in Racket Gamble.

  https://dtai.cs.kuleuven.be/problog/tutorial/basic/06_more_features.html
  """
  Suppose we are packing our bags to go on a trip. We have a set of items, each having a 
  particular weight, and we pack each item with probability inversely proportional to its 
  weight. We want to compute the probability that we will have excess baggage, i.e., 
  that the total weight of our baggage will exceed a given limit. We can model this with 
  the following ProbLog program.

  Note that this program uses several Prolog builtins such as support for lists and 
  arithmetic. The program also uses another feature of ProbLog2, namely support for 
  (intensional) probabilistic facts with a `flexible’ probability. This means that the 
  probability is not prespecified but is an arithmetic expression that needs to be 
  computed. In the program, this is used in the intensional probabilistic 
  fact “P::pack(Item) :- …”, which says that the probability of packing an item is 
  inversely proportional to its weight. Such a flexible probability can be used in 
  ProbLog2 under the restriction that the arithmetic expression can be evaluated at 
  call-time (i.e., by the time the probabilistic fact is reached by SLD resolution to 
  prove the queries and evidence).

  ... 

  (ProbLog code:)
  weight(skis,6).
  weight(boots,4).
  weight(helmet,3).
  weight(gloves,2).
  
  % intensional probabilistic fact with flexible probability:
  P::pack(Item) :- weight(Item,Weight),  P is 1.0/Weight.
  
  excess(Limit) :- excess((skis,boots,helmet,gloves),Limit). % all possible items
  excess((),Limit) :- Limit<0.
  excess((I|R),Limit) :- pack(I), weight(I,W), L is Limit-W, excess(R,L).
  excess((I|R),Limit) :- \+pack(I), excess(R,Limit).
  query(excess(8)). % 0.11805556  
  """
  
  Note: This not the same approach as the ProbLog model above, it's actually more like 
  MiniZinc than Prolog.

var : excess limit
#f: 127/144 (0.8819444444444444)
#t: 17/144 (0.11805555555555555)
mean: 17/144 (0.11805555555555555)

var : limit
8.0: 1 (1.0)
mean: 8.0

var : sumWeights
0: 5/24 (0.20833333333333334)
2: 5/24 (0.20833333333333334)
6: 1/9 (0.1111111111111111)
3: 5/48 (0.10416666666666667)
5: 5/48 (0.10416666666666667)
4: 5/72 (0.06944444444444445)
9: 1/18 (0.05555555555555555)
8: 1/24 (0.041666666666666664)
7: 5/144 (0.034722222222222224)
11: 1/48 (0.020833333333333332)
10: 1/72 (0.013888888888888888)
12: 1/72 (0.013888888888888888)
13: 1/144 (0.006944444444444444)
15: 1/144 (0.006944444444444444)
mean: 4 (4.0)

var : selected items
(0 ()): 5/24 (0.20833333333333334)
(2 ((gloves 2 #t))): 5/24 (0.20833333333333334)
(3 ((helmet 3 #t))): 5/48 (0.10416666666666667)
(5 ((helmet 3 #t) (gloves 2 #t))): 5/48 (0.10416666666666667)
(6 ((boots 4 #t) (gloves 2 #t))): 5/72 (0.06944444444444445)
(4 ((boots 4 #t))): 5/72 (0.06944444444444445)
(8 ((skis 6 #t) (gloves 2 #t))): 1/24 (0.041666666666666664)
(6 ((skis 6 #t))): 1/24 (0.041666666666666664)
(9 ((boots 4 #t) (helmet 3 #t) (gloves 2 #t))): 5/144 (0.034722222222222224)
(7 ((boots 4 #t) (helmet 3 #t))): 5/144 (0.034722222222222224)
(11 ((skis 6 #t) (helmet 3 #t) (gloves 2 #t))): 1/48 (0.020833333333333332)
(9 ((skis 6 #t) (helmet 3 #t))): 1/48 (0.020833333333333332)
(12 ((skis 6 #t) (boots 4 #t) (gloves 2 #t))): 1/72 (0.013888888888888888)
(10 ((skis 6 #t) (boots 4 #t))): 1/72 (0.013888888888888888)
(15 ((skis 6 #t) (boots 4 #t) (helmet 3 #t) (gloves 2 #t))): 1/144 (0.006944444444444444)
(13 ((skis 6 #t) (boots 4 #t) (helmet 3 #t))): 1/144 (0.006944444444444444)


  If we constraint to not exess the limit:

var : excess limit
#f: 1 (1.0)
mean: 0 (0.0)

var : limit
8.0: 1 (1.0)
mean: 8.0

var : sumWeights
2: 30/97 (0.30927835051546393)
6: 16/97 (0.16494845360824742)
3: 15/97 (0.15463917525773196)
5: 15/97 (0.15463917525773196)
4: 10/97 (0.10309278350515463)
8: 6/97 (0.061855670103092786)
7: 5/97 (0.05154639175257732)
mean: 399/97 (4.11340206185567)

var : selected items
(2 ((gloves 2 #t))): 30/97 (0.30927835051546393)
(5 ((helmet 3 #t) (gloves 2 #t))): 15/97 (0.15463917525773196)
(3 ((helmet 3 #t))): 15/97 (0.15463917525773196)
(6 ((boots 4 #t) (gloves 2 #t))): 10/97 (0.10309278350515463)
(4 ((boots 4 #t))): 10/97 (0.10309278350515463)
(6 ((skis 6 #t))): 6/97 (0.061855670103092786)
(8 ((skis 6 #t) (gloves 2 #t))): 6/97 (0.061855670103092786)
(7 ((boots 4 #t) (helmet 3 #t))): 5/97 (0.05154639175257732)

  This is a port of my WebPPL model trip.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (enumerate 
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define limit 8.0)
   ;; (define limit 4.0)
   ;; (define limit 10.0)
   ;; (define limit 18.0)
   
   (define items '("skis" "boots" "helmet" "gloves"))
   ;; (define weights = (skis: 6,
   ;;                          boots: 4,
   ;;                          helmet: 3,
   ;;                          gloves: 2
   ;;                          ); 
   (define weights (make-hash '(("skis" 6)
                                ("boots" 4)
                                ("helmet" 3)
                                ("gloves" 2))))
   
   (define (getWeight i)
     (car (hash-ref weights i))
     )
   
   (define selected (mem (lambda (i)
                           (flip (/ (getWeight i))))))
   
   (define sumWeights
     (for/sum ([i items])
       (* (getWeight i) (boolean->integer (selected i)))))
   
   ;; We want the probability the we excess the weight.
   (define excess (mem (lambda(v) (> sumWeights v ))))
   
   ;; (name,weight,selected)
   (define items_with_weights
     (for/list ([i items])
       (list i (getWeight i) (selected i))))

   ;; Here we can enforce that we don't excess the weights
   ; (observe/fail (and (> sumWeights 0) (not (excess limit))))
   
   (list (excess limit)
         limit
         sumWeights
         ;; items_with_weights:items_with_weights,
         ; (sumWeights,filter(function(i) ( i(2) ),items_with_weights))
         (list sumWeights (filter (lambda (i) (list-ref i 2)) items_with_weights))
        )
   
   )
  )

(show-marginals (model)
                (list "excess limit"
                      "limit"
                      "sumWeights"
                      "selected items"
                      )
                #:num-samples 1000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.84
                ; #:show-stats? #t
                )
