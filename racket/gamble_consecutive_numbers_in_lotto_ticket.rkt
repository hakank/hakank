#| 

  Consecutive numbers in Lotto ticket in Racket.Gamble 

  From https://www.reddit.com/r/Probability/comments/1etgees/consecutive_numbers_in_lotto_random_pick_ticket/
  """
  Consecutive numbers in lotto 'random pick' ticket

  Just got this lotto ticket. Thought the number of consecutive numbers was 
  just way too many. Are these really random? Or is this number of consecutive numbers normal? 
  """

  Here the Lotto is 7 numbers in 1..40.

  * From model1:
  For a single Lotto ticket row, the probabilities of the 
  number of consecutive numbers are:

  var : num-consecutive
  1: 0.43105
  0: 0.2876
  2: 0.22682
  3: 0.0494
  4: 0.00494
  5: 0.00019
  mean: 1.0536

  var : p
  #t: 0.7124
  #f: 0.2876
  mean: 0.7124

  However, the example shows that for the 18 rows in the Lotto ticket,
  17 of these rows has at least one occurrence of consecutive numbers.

  Now, the model shows that the probability of at least one occurrence is quite 
  (surprisingly?) high: about 71%.  

  So what is the probability of 17 ticket rows of 18 has at least one occurrence
  of consecutive numbers. Here's a simple way, using binomial. First a simulation:

   > (show-freq (repeat (lambda () (binomial 18 0.7124)) 100000))
  (13 : 0.20418937159426087)
  (14 : 0.18122281657751338)
  (12 : 0.17905314202869568)
  (11 : 0.12318152277158426)
  (15 : 0.12113183022546618)
  (10 : 0.06928960655901614)
  (16 : 0.05534169874518822)
  (9 : 0.03220516922461631)
  (17 : 0.016277558366245063)
  (8 : 0.011448282757586362)
  (7 : 0.003329500574913763)
  (18 : 0.00226965955106734)
  (6 : 0.0008898665200219967)
  (5 : 0.00013997900314952758)
  (3 : 2.9995500674898764e-5)
  (mean: 12.81900714892766)


  Given that we can rely on p=0.7124 in the experiment, the 
  exact PDF for exactly 17 such ticket rows is thus
  > (dist-pdf (binomial-dist 18 0.714) 17)
  0.016769037596603788

  Here are the exact probabilities for 0..18 rows with at least one occurrence 
  of consecutive numbers (again, assuming that p=0.714 is correct):
  > (map (lambda (v) (list v (dist-pdf (binomial-dist 18 0.714) v))) (range 19))
  '((0 1.6390363915432855e-10)
    (1 7.365348148291715e-9)
    (2 1.5629474794399444e-7)
    (3 2.0810153852123455e-6)
    (4 1.948223319547571e-5)
    (5 0.00013618489721816445)
    (6 0.0007366364894982531)
    (7 0.003152598122887628)
    (8 0.010821899325681568)
    (9 0.030018788339303177)
    (10 0.06744780904488187)
    (11 0.12246086626330747)
    (12 0.1783389888065299)
    (13 0.20548795805464598)
    (14 0.18321478777599556)
    (15 0.1219723622117117)
    (16 0.057094580388436726)
    (17 0.016769037596603788)
    (18 0.0023257756107177745))

  The probability of (exact) 17 such row is small, about 1.7%, but 
  it's definity not very rare, though given a "surprise level" 0.01
  (i.e. how rare an event should in order to raise a surprise),
  it _is_ a little surprising.

  However, we would probably also be surprised if there were - say - 15 or 
  more such consecutive rows. And the probability of 15 or more 
  such rows is quite high: almost 20%!

  > (- 1 (dist-cdf (binomial-dist 18 0.714) 14))
  0.1981617558074703


  The average number of ticket rows (of 18) with at least one occurrence is
  > (dist-mean (binomial-dist 18 0.714))
  12.852

  So, we should expect quite many occurrences of consecutive numbers in 
  a given Lotto ticket.


  * Other Lottos.

  For the Swedish Lotto variant a draw is to pick 7 numbers from 1..35.
  The probability of at least one occurrence of consecutive numbers is 
  about 76%

  var : num-consecutive
  1: 0.42292
  2: 0.26316
  0: 0.23527
  3: 0.07007
  4: 0.00813
  5: 0.00044
  6: 1e-5
  mean: 1.1942300000000001

  var : p
  #t: 0.7647299999999999
  #f: 0.23527
  mean: 0.7647299999999999

  For the UK Lotto with 6 numbers from 1..49, the probability is lower: about 49.6%
  var : num-consecutive
  0: 0.50328
  1: 0.3891
  2: 0.09796
  3: 0.00934
  4: 0.00032
  mean: 0.61432

  var : p
  #f: 0.50328
  #t: 0.49672
  mean: 0.49672

  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num 40)
   (define nums (range 1 (add1 num)))
   (define num-pick 7)

   (define picks (sort (draw-without-replacement num-pick nums) <))
   (define diffs (differences picks))
   
   ; Here we count any occurrence of two consecutive numbers as an occurrence.
   ; This mean that if three numbers are consecutive, it's counted as 2 occurrences, etc.
   ;; (define num-conscutive0 (for/sum ([i (range 1 num-pick)])
   ;;                          (if (= (- (list-ref picks i) (list-ref picks (sub1 i))) 1) 1 0)))
   (define num-conscutive (for/sum ([v diffs]) (if (= v 1) 1 0)))
   
   ; Probability of at least one occurrence
   (define p (> num-conscutive 0))   

   ; (show2 "picks" picks "num-conscutive" num-conscutive "p" p)
   
   (list num-conscutive
         p
         )
                                    

   )
)

(show-marginals (model1)
                (list  "num-consecutive"
                       "p"
                     )
              #:num-samples 100000
              ; #:truncate-output 5
              ; #:skip-marginals? #t
              ; #:show-stats? #t
              ; #:credible-interval 0.84
              ; #:show-histogram? #t
              ; #:show-percentiles? #t
              )

