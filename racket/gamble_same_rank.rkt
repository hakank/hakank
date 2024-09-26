#| 

  Same rank in Racket.Gamble 

  From Gunnar Blom, Lars Holst, Dennis Sandell:
  "Problems and Snapshots from the World of Probability"
  Page 22.
  """
  100 students are seating (randomly) on seats numbered 0 to 99. 
  The lectures says: assume that I rank you according to your age. 
  How many will have the same rank number as the seat number?
  """

  Here are four different approaches to this problem which all
  give the same expectation, i.e. E(same_rank) = 1.

n=100 Model1 (permutation-dist)
var : numSameRank
0: 0.36960000000000004
1: 0.36740000000000006
2: 0.18280000000000002
3: 0.06010000000000001
4: 0.016300000000000002
5: 0.0032000000000000006
6: 0.0005000000000000001
7: 0.00010000000000000002
mean: 0.9982000000000002

n = 6 Model2 (no all different) 
var : numSameRank
1: 3125/7776 (0.4018775720164609)
0: 15625/46656 (0.33489797668038407)
2: 3125/15552 (0.20093878600823045)
3: 625/11664 (0.05358367626886145)
4: 125/15552 (0.008037551440329218)
5: 5/7776 (0.0006430041152263374)
6: 1/46656 (2.143347050754458e-5)
mean: 1 (1.0)


n = 6 Model2 (with all different) 
var : numSameRank
0: 53/144 (0.3680555555555556)
1: 11/30 (0.36666666666666664)
2: 3/16 (0.1875)
3: 1/18 (0.05555555555555555)
4: 1/48 (0.020833333333333332)
6: 1/720 (0.001388888888888889)
mean: 1 (1.0)


n = 100 Model2 (no all different) 
var : numSameRank
1: 0.374
0: 0.368
2: 0.187
3: 0.05
4: 0.019
5: 0.001
6: 0.001
mean: 0.985


n=6 Model3 (flip 1/n) enum 
var : numSameRank
1: 3125/7776 (0.4018775720164609)
0: 15625/46656 (0.33489797668038407)
2: 3125/15552 (0.20093878600823045)
3: 625/11664 (0.05358367626886145)
4: 125/15552 (0.008037551440329218)
5: 5/7776 (0.0006430041152263374)
6: 1/46656 (2.143347050754458e-5)
mean: 1 (1.0)


n=100 Model3 (flip 1/n) importance-sampler (100000 samples)
var : numSameRank
1: 0.36957999999999996
0: 0.36615999999999993
2: 0.18500999999999998
3: 0.06071999999999999
4: 0.015149999999999997
5: 0.0027499999999999994
6: 0.0005299999999999999
7: 8.999999999999999e-5
8: 9.999999999999999e-6
mean: 0.9999999999999999

  This is a port of my WebPPL model same_rank.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;
; Using permutation-dist
; 
(define (model1 n)
  (; enumerate ; cannot handle permutation-dist
   importance-sampler

   (define x (sample (permutation-dist n)))
   (define numSameRank (for/sum ((i n))
                         (boolean->integer (= (vector-ref x i) i))))
   (list numSameRank)

   )
)

(displayln "n=100 Model1 (permutation-dist)")
(show-marginals (model1 100)
              (list  "numSameRank")
                    #:num-samples 10000)


;
; Poor man's all different
;
(define (all_different a n)
  (for ((i n))
    (for ((j i)) 
      (when (not (= i j))
        (observe/fail (not (= (list-ref a i) (list-ref a j)))))))
  )
                
;
; Generate random-integer n
; With / without all_difference
;
(define (model2-enum n alldiff)
  (enumerate 

   (defmem (x i) (random-integer n))
   (define xs (for/list ((i n)) (x i)))
   (when alldiff (all_different xs n))
   
   (define numSameRank (for/sum ((i n))
                         (boolean->integer (= (x i) i))))
   
   (list numSameRank
         )

   )
)

(define (model2-importance-sampler n alldiff)
  (importance-sampler

   (defmem (x i) (random-integer n))
   (define xs (for/list ((i n)) (x i)))
   (when alldiff (all_different xs n))   
   (define numSameRank (for/sum ((i n))
                         (boolean->integer (= (x i) i))))
   
   (list numSameRank
         )

   )
)

(displayln "n = 6 Model2 (no all different) ")
(show-marginals (model2-enum 6 #f)
                (list  "numSameRank"))
(newline)

(displayln "n = 6 Model2 (with all different) ")
(show-marginals (model2-enum 6 #t)
                (list  "numSameRank"))
(newline)

(displayln "n = 100 Model2 (no all different) ")
(show-marginals (model2-importance-sampler 100 #f)
                (list  "numSameRank")
                #:num-samples 1000)
(newline)

;
; Model 3
; Generate n 0/1 with probability 1/n for getting a 1
;
(define (model3-enum n)
  (enumerate

   (define p (/ 1 n))
   (defmem (x i) (bernoulli p))
   (define numSameRank (for/sum ([i n]) (x i)))
   (list numSameRank)
   )
)


(displayln "n=6 Model3 (flip 1/n) enum ")
(show-marginals (model3-enum 6)
              (list  "numSameRank"))
(newline)

(define (model3-importance-sampler n)
  (; enumerate ; cannot handle permutation-dist
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define p (/ 1 n))
   (defmem (x i) (bernoulli p))
   (define numSameRank (for/sum ([i n]) (x i)))
   (list numSameRank)
   )
)

(displayln "n=100 Model3 (flip 1/n) importance-sampler (100000 samples)")
(show-marginals (model3-importance-sampler 100)
                (list  "numSameRank")
                #:num-samples 100000)
(newline)
