#| 

  Probability of winning an election with a < 50% forecaseted probability of winning in Racket/Gamble 

  What is the probability of B winning of A when polls says that A has > 50% chance of winning? 

  Examples:
  * A is forecasted to win with 51%. What is the probability that B wins?
    (prob 51/100) -> 2401/10000 (0.2401)

    That is perhaps a little surprising, but remember that the probability of 
    A winning is not very large either. 

    In the first case (A is forecasted to with with 51%): 
    - The probability that A wins is 0.2601. 
    - The probability that B wins is 0.2401
    - Most cases are ties, with probability 0.4998

  * A is forecased to win with 60%. What is the probability that B wins?
    (prob 60/100) -> 4/25 (0.16)

    Here is a little more gap between A and B:
    - The probability that A wins is 0.36
    - The probability that B wins is 0.16
    - Most cases are ties, with probability 0.48

    As in the first case, there's a lot of ties.

  Below are the cases where A is forcasted to win with probability 0.5..0.99:
    - probability that A wins
    - probability that B wins
    - probability of a tie

  (This uses the theoretical form defined below.)

  (0.5 a wins: 0.25 b wins: 0.25 tie: 0.5)
  (0.51 a wins: 0.2601 b wins: 0.24009999999999998 tie: 0.4998)
  (0.52 a wins: 0.27040000000000003 b wins: 0.2304 tie: 0.4992)
  (0.53 a wins: 0.28090000000000004 b wins: 0.22089999999999999 tie: 0.4982)
  (0.54 a wins: 0.2916 b wins: 0.21159999999999995 tie: 0.4968)
  (0.55 a wins: 0.30250000000000005 b wins: 0.20249999999999996 tie: 0.495)
  (0.56 a wins: 0.31360000000000005 b wins: 0.19359999999999997 tie: 0.49279999999999996)
  (0.5700000000000001 a wins: 0.3249000000000001 b wins: 0.18489999999999995 tie: 0.49019999999999997)
  (0.5800000000000001 a wins: 0.3364000000000001 b wins: 0.17639999999999995 tie: 0.48719999999999997)
  (0.5900000000000001 a wins: 0.3481000000000001 b wins: 0.16809999999999994 tie: 0.48379999999999995)
  (0.6000000000000001 a wins: 0.3600000000000001 b wins: 0.15999999999999992 tie: 0.48)
  (0.6100000000000001 a wins: 0.3721000000000001 b wins: 0.15209999999999993 tie: 0.47579999999999995)
  (0.6200000000000001 a wins: 0.38440000000000013 b wins: 0.14439999999999992 tie: 0.47119999999999995)
  (0.6300000000000001 a wins: 0.39690000000000014 b wins: 0.1368999999999999 tie: 0.46619999999999995)
  (0.6400000000000001 a wins: 0.4096000000000002 b wins: 0.1295999999999999 tie: 0.46079999999999993)
  (0.6500000000000001 a wins: 0.42250000000000015 b wins: 0.1224999999999999 tie: 0.4549999999999999)
  (0.6600000000000001 a wins: 0.4356000000000002 b wins: 0.1155999999999999 tie: 0.4487999999999999)
  (0.6700000000000002 a wins: 0.4489000000000002 b wins: 0.1088999999999999 tie: 0.44219999999999987)
  (0.6800000000000002 a wins: 0.4624000000000002 b wins: 0.1023999999999999 tie: 0.43519999999999986)
  (0.6900000000000002 a wins: 0.47610000000000025 b wins: 0.0960999999999999 tie: 0.42779999999999985)
  (0.7000000000000002 a wins: 0.49000000000000027 b wins: 0.0899999999999999 tie: 0.4199999999999999)
  (0.7100000000000002 a wins: 0.5041000000000002 b wins: 0.0840999999999999 tie: 0.41179999999999983)
  (0.7200000000000002 a wins: 0.5184000000000003 b wins: 0.07839999999999989 tie: 0.40319999999999984)
  (0.7300000000000002 a wins: 0.5329000000000003 b wins: 0.0728999999999999 tie: 0.39419999999999983)
  (0.7400000000000002 a wins: 0.5476000000000003 b wins: 0.06759999999999988 tie: 0.3847999999999998)
  (0.7500000000000002 a wins: 0.5625000000000003 b wins: 0.06249999999999989 tie: 0.3749999999999998)
  (0.7600000000000002 a wins: 0.5776000000000003 b wins: 0.05759999999999989 tie: 0.36479999999999974)
  (0.7700000000000002 a wins: 0.5929000000000003 b wins: 0.05289999999999989 tie: 0.35419999999999974)
  (0.7800000000000002 a wins: 0.6084000000000004 b wins: 0.04839999999999989 tie: 0.3431999999999997)
  (0.7900000000000003 a wins: 0.6241000000000004 b wins: 0.04409999999999989 tie: 0.3317999999999997)
  (0.8000000000000003 a wins: 0.6400000000000005 b wins: 0.0399999999999999 tie: 0.3199999999999997)
  (0.8100000000000003 a wins: 0.6561000000000005 b wins: 0.036099999999999896 tie: 0.3077999999999997)
  (0.8200000000000003 a wins: 0.6724000000000004 b wins: 0.0323999999999999 tie: 0.29519999999999963)
  (0.8300000000000003 a wins: 0.6889000000000005 b wins: 0.0288999999999999 tie: 0.2821999999999996)
  (0.8400000000000003 a wins: 0.7056000000000006 b wins: 0.025599999999999904 tie: 0.2687999999999996)
  (0.8500000000000003 a wins: 0.7225000000000005 b wins: 0.022499999999999905 tie: 0.25499999999999956)
  (0.8600000000000003 a wins: 0.7396000000000006 b wins: 0.01959999999999991 tie: 0.24079999999999954)
  (0.8700000000000003 a wins: 0.7569000000000006 b wins: 0.016899999999999915 tie: 0.2261999999999995)
  (0.8800000000000003 a wins: 0.7744000000000006 b wins: 0.01439999999999992 tie: 0.2111999999999995)
  (0.8900000000000003 a wins: 0.7921000000000006 b wins: 0.012099999999999923 tie: 0.19579999999999945)
  (0.9000000000000004 a wins: 0.8100000000000006 b wins: 0.009999999999999929 tie: 0.17999999999999944)
  (0.9100000000000004 a wins: 0.8281000000000006 b wins: 0.008099999999999934 tie: 0.1637999999999994)
  (0.9200000000000004 a wins: 0.8464000000000007 b wins: 0.0063999999999999405 tie: 0.1471999999999994)
  (0.9300000000000004 a wins: 0.8649000000000007 b wins: 0.004899999999999947 tie: 0.13019999999999934)
  (0.9400000000000004 a wins: 0.8836000000000007 b wins: 0.003599999999999953 tie: 0.11279999999999932)
  (0.9500000000000004 a wins: 0.9025000000000007 b wins: 0.00249999999999996 tie: 0.09499999999999928)
  (0.9600000000000004 a wins: 0.9216000000000008 b wins: 0.0015999999999999673 tie: 0.07679999999999924)
  (0.9700000000000004 a wins: 0.9409000000000008 b wins: 0.0008999999999999749 tie: 0.05819999999999922)
  (0.9800000000000004 a wins: 0.9604000000000008 b wins: 0.00039999999999998294 tie: 0.03919999999999918)
  (0.9900000000000004 a wins: 0.9801000000000009 b wins: 9.999999999999129e-5 tie: 0.019799999999999148)



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;
; Closed form
;
; Probabillity of A winning with a forecasted probability of a for A winning
(define (prob-a-wins a) (expt a 2))

; Probabillity of B winning with a forecasted probability of a for A winning
(define (prob-b-wins a) (expt (- a 1) 2))

; Probabillity of a tie with a forecasted probability of a for A winning
(define (prob-tie a) (- (* 2 (- a 1) a)))

(for ([a (range 0.5 1 0.01)])
      (show2 a "a wins:" (prob-a-wins a) "b wins:" (prob-b-wins a) "tie:" (prob-tie a) ))
(newline)


#|
  Checking the calculations, for a-prob 51/100

  variable : a-wins
  #f: 7399/10000 (0.7399)
  #t: 2601/10000 (0.2601)
  mean: 2601/10000 (0.2601)

  variable : b-wins
  #f: 7599/10000 (0.7599)
  #t: 2401/10000 (0.2401)
  mean: 2401/10000 (0.2401)

  variable : tie
  #f: 2501/5000 (0.5002)
  #t: 2499/5000 (0.4998)
  mean: 2499/5000 (0.4998)

  variable : a-wins theoretical
  2601/10000: 1 (1.0)
  mean: 2601/10000 (0.2601)

  variable : b-wins theoretical
  2401/10000: 1 (1.0)
  mean: 2401/10000 (0.2401)

  variable : tie theoretical
  2499/5000: 1 (1.0)
  mean: 2499/5000 (0.4998)


|#
(define (model [a-prob (/ 51 100)])
  (enumerate

   (define p1 (bernoulli a-prob))
   (define p2 (bernoulli (- 1 a-prob)))
   
   (define a-wins (> p1 p2))
   (define b-wins (> p2 p1))
   (define tie (= p2 p1))

   ; Theoretical values
   (define a-wins-t (prob-a-wins a-prob))
   (define b-wins-t (prob-b-wins a-prob))
   (define tie-t (prob-tie a-prob))

   ; If we rule out the ties, then the probabilities
   ; are:
   ; * probability of A wins: a-prob
   ; * probability of B wins: 1-a-prob
   ; (observe/fail (eq? tie #f))

   (list a-wins
         b-wins
         tie
         a-wins-t
         b-wins-t
         tie-t
         )
   
   )
)

(show-marginals (model)
                (list "a-wins"
                      "b-wins"
                      "tie"
                      "a-wins theoretical"
                      "b-wins theoretical"
                      "tie theoretical"                      
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


