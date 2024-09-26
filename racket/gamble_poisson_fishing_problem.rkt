#| 

  Poisson fishing problem in Racket.Gamble 

  https://www.reddit.com/r/probabilitytheory/comments/1fgmhgw/poisson_fishing_problem/
  """
  Question goes like this: A fisherman catches fish according to a Poisson process with 
  rate 0.6 per hour. The fisherman will keep fishing for two hours. If he has caught at 
  least one fish, he quits. Otherwise, he continues until he catches at least one fish.

  (a) Find the probability that the total time he spends fishing is between two and five hours.

  Solution and my conflicting approach:

  First of all he'll fish for more than 2 hrs if he catches no fish in first two hrs and the 
  probability of that is P(k=0,t=2).

  1.After two hrs, the probability that he fish for 3 more hrs is that he gets 1 fish in the 
  interval of 3 hrs which is P(k=1,t=3). So total probability is P1 = P(k=0,t=2).P(k=1,t=3)

  2. After 2 hrs, the probability that waiting time is less than 3hrs is P(0<T<3) = 1-exp(0.63) 
  (from exponential pdf). This is equivalent to saying there is atleast one fish caught in 
  3hrs interval which is equal to 1-P(k=0,t=3) = 1-exp(0.63. So the total probability is 
  now P2 = P(k=0,t=2)[1 - P(k=0,t=3)]

  You can see the results ate different but approach seems to me is correct. Can you please 
  clarify the results. Thank you.

  P.S. P(k,t) means k arrival in t interval
  """

  This model: The probability that the fishing is - according to this model - between 
  2 and 5 hours is very close to  0.5 (perhaps exactly 0.5?).

  * importance-sampler (1000000 samples):

  var : len
  1: 0.45199600000000023
  2: 0.24733400000000016
  3: 0.13537500000000008
  4: 0.07449500000000003
  5: 0.040999000000000015
  6: 0.022400000000000017
  7: 0.01235500000000001
  8: 0.006793000000000004
  9: 0.0037650000000000023
  10: 0.002037000000000001
  11: 0.0010820000000000007
  12: 0.0006050000000000004
  13: 0.00035800000000000025
  14: 0.00018200000000000011
  15: 0.00010900000000000006
  16: 4.0000000000000024e-5
  17: 2.9000000000000017e-5
  18: 1.900000000000001e-5
  19: 1.3000000000000008e-5
  20: 8.000000000000005e-6
  21: 4.000000000000002e-6
  24: 2.000000000000001e-6
  mean: 2.2152610000000017

  var : total
  1: 0.7293630000000003
  2: 0.21906900000000018
  3: 0.04397300000000002
  4: 0.006685000000000004
  5: 0.0008310000000000005
  6: 7.000000000000002e-5
  7: 9.000000000000005e-6
  mean: 1.3307980000000006

  var : p
  #f: 0.5017969999999999
  #t: 0.4982030000000002
  mean: 0.4982030000000002


  * Enumerate #:limit 1e-10

  var : len
  1: 0.4511883639481452
  2: 0.24761742420496868
  3: 0.13589532370331744
  4: 0.07458093493914494
  5: 0.040930884920849224
  ...
  38: 9.776432708103651e-11
  39: 5.3654200296975105e-11
  40: 2.9446049448299463e-11
  41: 1.2431026595559086e-11
  42: 6.8222920442371276e-12
  mean: 2.2163692128300974

  var : total
  1: 0.7298215291606677
  2: 0.21894645874242427
  3: 0.04378929174013725
  4: 0.006568393753445583
  5: 0.0007882072449143398
  6: 7.882071788802125e-5
  7: 6.756055294066218e-6
  8: 5.066943251566928e-7
  9: 3.377271469058299e-8
  10: 2.0176643562467555e-9
  11: 1.0052453494916454e-10
  mean: 1.3298215285901156

  var : p
  #f: 0.5009754322317193
  #t: 0.4990245677682801
  mean: 0.4990245677682801

  * Enumerate #:limit 1e-100

  var : p
  #f: 0.500975432273835
  #t: 0.4990245677261637
  mean: 0.4990245677261637

  * Enumerate #:limit 1e-1000
  var : p
  #f: 0.500975432273835
  #t: 0.4990245677261637
  mean: 0.4990245677261637

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-100000
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define lambda_ 6/10)

   (define fishing (for/list ([t (in-naturals)]
                              #:do [(define num-fish (poisson lambda_))]
                              #:final (>= num-fish 1))
                     num-fish)
     )

   (define len (length fishing))
   (define total (sum fishing))

   (define p (and (>= len 2) (<= len 5)))
   
   (list ; fishing
         len
         total
         p
         )
   
   )
)

(show-marginals (model)
                (list  ; "fishing"
                       "len"
                       "total"
                       "p"
                       )
                #:num-samples 1000000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


