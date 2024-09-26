#| 

  Messing with envelopes in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/205
  """
  There are n letters and n envelopes. You put the letters randomly in the 
  envelopes so that each letter is in one envelope. (Effectively a random permutation of 
  n numbers chosen uniformly). Calculate the expected number of envelopes with the correct 
  letter inside them.

  Answer: 1
  """

 * n = 100 (importance-sampler)

  var : p
  1: 0.36903
  0: 0.36572
  2: 0.18412
  3: 0.06235
  4: 0.0152
  5: 0.00295
  6: 0.00054
  7: 8e-5
  8: 1e-5
  mean: 1.00375


 * n = 10 enumerate
  0: 16481/44800 (0.3678794642857143)
  1: 16687/45360 (0.36787918871252206)
  2: 2119/11520 (0.18394097222222222)
  3: 103/1680 (0.06130952380952381)
  4: 53/3456 (0.015335648148148149)
  5: 11/3600 (0.0030555555555555557)
  6: 1/1920 (0.0005208333333333333)
  7: 1/15120 (6.613756613756614e-5)
  8: 1/80640 (1.240079365079365e-5)
  10: 1/3628800 (2.755731922398589e-7)
  mean: 1 (1.0)

  Note: 1/exp(1) = 0.36787944117144233

 * n = 6 enumerate
  var : p
  0: 53/144 (0.3680555555555556)
  1: 11/30 (0.36666666666666664)
  2: 3/16 (0.1875)
  3: 1/18 (0.05555555555555555)
  4: 1/48 (0.020833333333333332)
  6: 1/720 (0.001388888888888889)
  mean: 1 (1.0)

  * n = 8 enumerate
  var : p
  0: 2119/5760 (0.36788194444444444)
  1: 103/280 (0.3678571428571429)
  2: 53/288 (0.1840277777777778)
  3: 11/180 (0.06111111111111111)
  4: 1/64 (0.015625)
  5: 1/360 (0.002777777777777778)
  6: 1/1440 (0.0006944444444444445)
  8: 1/40320 (2.48015873015873e-5)
  mean: 1 (1.0)

  * Probabilities of the number of matches

  The probability of the number of matches are quite similar whether 
  n is 5, 10, 100, 500, and 1000:

  n: 5
  var : p
  1: 0.37571000000000004
  0: 0.36515000000000003
  2: 0.16665000000000002
  3: 0.08412000000000001
  5: 0.008370000000000002
  mean: 1.0032200000000002

  n: 10
  var : p
  1: 0.36715000000000003
  0: 0.36679000000000006
  2: 0.18521000000000004
  3: 0.061560000000000004
  4: 0.015380000000000001
  5: 0.0031800000000000005
  6: 0.0005900000000000001
  7: 0.00012000000000000002
  8: 2.0000000000000005e-5
  mean: 1.00421

  n: 100
  var : p
  0: 0.36717000000000005
  1: 0.36638000000000004
  2: 0.18657000000000004
  3: 0.061460000000000015
  4: 0.014990000000000003
  5: 0.0029300000000000007
  6: 0.00040000000000000013
  7: 0.00010000000000000003
  mean: 1.0016100000000001

  n: 500
  var : p
  1: 0.36737
  0: 0.36559
  2: 0.18635
  3: 0.06202
  4: 0.01518
  5: 0.00295
  6: 0.0004
  7: 0.00012
  8: 2e-5
  mean: 1.005

  n: 1000
  var : p
  1: 0.36903
  0: 0.36572
  2: 0.18412
  3: 0.06235
  4: 0.0152
  5: 0.00295
  6: 0.00054
  7: 8e-5
  8: 1e-5
  mean: 1.00375

  * Poisson distribution 

  Compare with the Poisson distribution for lambda = 1
  > (map (lambda (v) (list v (dist-pdf (poisson-dist 1 ) v))) (range 20))
  '((0 0.36787944117144233)
    (1 0.3678794411714424)
    (2 0.18393972058572117)
    (3 0.06131324019524039)
    (4 0.015328310048810098)
    (5 0.0030656620097620196)
    (6 0.0005109436682936699)
    (7 7.299195261338141e-5)
    (8 9.123994076672677e-6)
    (9 1.0137771196302974e-6)
    (10 1.0137771196302974e-7)
    (11 9.216155633002704e-9)
    (12 7.680129694168921e-10)
    (13 5.907792072437631e-11)
    (14 4.2198514803125934e-12)
    (15 2.8132343202083955e-13)
    (16 1.7582714501302472e-14)
    (17 1.0342773236060278e-15)
    (18 5.745985131144598e-17)
    (19 3.0242027006024205e-18))


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model n)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; Use with importance-sampler (do not use with enumerate)
   (define letter (shuffle (range n)))

   ; Use with enumerate and only for small n
   ; (define letter (draw-without-replacement n (range n)))

   ; Use importance-sampler
   ; (define letter (vector->list (sample (permutation-dist n))))
   
   (define p (for/sum ([i n]) (if (= (list-ref letter i) i) 1 0)))

  
   (list p)

   )
)

(for ([n '(5 10 50 100 200 500 1000)])
  (show "n" n)
  (show-marginals (model n)
                  (list  "p"
                         )
                  #:num-samples 100000
                    ))


