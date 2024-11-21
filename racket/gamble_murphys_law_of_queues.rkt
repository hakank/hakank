#| 

  Murphy's law of queues in Racket/Gamble 

  From Robert Matthews (link not available anymore), cited by my 
  (Swedish) simulating page:
  """
  Murphy's law of queues - The line next to you will usually finish first - is 
  also rooted in simple math.

  If you're waiting behind a person with a two-months supply of groceries, it's 
  hardly a surprise if you get through more slowly than your neighbours. But 
  what about joining a line that's identical in length from the ones on either 
  side of you. Is it all in your mind when you are the last one to get through?

  Actually...NO.

  Over time, it's true that on average the lines will move more or less at the 
  same rate, says Matthews. Each will suffer random delays when for example the 
  cashier runs out of change or a customer's cheque or bank card is declined. 
  But when we're lined up at any one particular time, we don't care about 
  averages, we care only about that one instance. In these cases, chances of you
  picking the fastest moving line is one out of the number of lines there are in
  the store. If there are 10 lines, you have a 1 in 10 chance (or 10%) of 
  picking the fastest line. Even if you're only concerned about beating the 
  lines on either side of you, you only have a 1 in 3 chance that you will. 
  You're more likely to be in a slower line.
  """

  Here's a simulation of this. 
  - There are 5 simular queues with 10 customers in each queue
  - I am last in one of them (picked randomly)
  - The service takes between 1 to 5 minutes per customer (randomly
    and independent of anything else)

  The probability that I stand in the *fastest* queue (p-first) is about 20%,
  i.e. as expected from 5 queues. The probability of being in one of 
  the 3 fastest queues (p-first3) is about 60%.

  Here is a run:

  var : fastest-queue
  1: 0.20450000000000024
  3: 0.20110000000000017
  2: 0.19870000000000013
  0: 0.19790000000000013
  4: 0.19780000000000014
  mean: 1.9964000000000015

  var : val-fastest
  25: 0.13930000000000012
  26: 0.13540000000000013
  24: 0.12390000000000011
  27: 0.10680000000000012
  23: 0.1060000000000001
  ...
  33: 0.0009000000000000008
  14: 0.0007000000000000005
  34: 0.0004000000000000004
  35: 0.0002000000000000002
  36: 0.0001000000000000001
  mean: 24.780800000000024

  var : p-first
  #f: 0.7965000000000004
  #t: 0.20350000000000024
  mean: 0.20350000000000024

  var : p-first3
  #t: 0.6041000000000004
  #f: 0.39590000000000036
  mean: 0.6041000000000004


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; Original problem
   (define num-queues 5)
   (define num-customers 10)
   (define max-time 5)
  
   (define my-queue (random-integer num-queues))
   
   (define q (for/list ([q num-queues])
               (for/list ([c num-customers]) (add1 (random-integer max-time)))))
   
   (define total (map sum q))
   (define total-sorted (arg-sort total))

   
   ; Note: we have to break ties randomly, otherwise
   ; 0 tends to be the fastest queue, then 1, etc.
   ; This is done with argmin2-random-ties
   ; (argmin2 picks the first occurrence).
   (define fastest-queue (argmin2-random-ties total))   
   (define val-fastest (list-ref total fastest-queue))
  
   (define p-first (= my-queue (first total-sorted)))
   (define p-first3
     (if (>= (length total-sorted) 3)
         (if (member my-queue (take total-sorted 3)) #t #f)
         #f
     ))
   
   (list fastest-queue
         val-fastest
         p-first
         p-first3
         )

   )
)

(displayln "Model1: Pick a random line")
(show-marginals (model)
                (list  "fastest-queue"
                       "val-fastest"
                       "p-first"
                       "p-first3"
                     )
                    #:num-samples 20000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


#|

  But what is I select the shortest queue?
  or rather: 
  Let's see what happens to the person that that is last in the 
  shortest queue.

  Here we have queues with a random number of customers.

  Now there's a pretty good chance that this person will be 
  served first (about 87%) and almost sure that they will be
  one of the first three to be served (99.8%).


  var : fastest-queue
  4: 0.20495000000000163
  3: 0.20135000000000153
  2: 0.20120000000000182
  1: 0.19650000000000156
  0: 0.19600000000000173
  mean: 2.0227500000000163
  HPD interval (0.84): 0..4

  var : val-fastest
  2: 0.07585000000000096
  1: 0.07105000000000092
  3: 0.0710000000000009
  4: 0.06505000000000082
  5: 0.0638000000000008
  ...
  84: 5.000000000000063e-5
  85: 5.000000000000063e-5
  86: 5.000000000000063e-5
  89: 5.000000000000063e-5
  93: 5.000000000000063e-5
  mean: 12.020600000000147
  HPD interval (0.84): 1..22

  var : p-first
  #t: 0.8724500000000076
  #f: 0.1275500000000005
  mean: 0.8724500000000076

  var : p-first3
  #t: 0.9975999999999992
  #f: 0.002400000000000029
  mean: 0.9975999999999992

  var : my-queue
  4: 0.20430000000000256
  2: 0.20190000000000202
  3: 0.19955000000000211
  1: 0.19795000000000165
  0: 0.19630000000000125
  mean: 2.0176000000000225
  HPD interval (0.84): 0..4



|#
(define (model2)
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num-queues 5)
   (define max-customers 20)
   (define max-time (add1 (random-integer 10)))

   
   (define num-customers (for/list ([i num-queues]) (add1 (random-integer max-customers))))

   ; Pick the queue with the smallest number of customers
   ; Or rather (to simplify the model): I happen to stand in the shortest line
   (define my-queue (argmin2-random-ties num-customers))

   (define q (for/list ([q num-queues])
               (for/list ([c (list-ref num-customers q)]) (add1 (random-integer max-time)))))
   
   (define total (map sum q))
   (define total-sorted (arg-sort total))
   
   ; Note: we have to break ties randomly, otherwise
   ; 0 tends to be the fastest queue, then 1, etc.
   ; This is done with argmin2-random-ties
   ; (argmin2 picks the first occurrence).
   (define fastest-queue (argmin2-random-ties total))   
   (define val-fastest (list-ref total fastest-queue))
  
   (define p-first (= my-queue (first total-sorted)))
   (define p-first3
     (if (>= (length total-sorted) 3)
         (if (member my-queue (take total-sorted 3)) #t #f)
         #f
     ))
  
  (list fastest-queue
        val-fastest
        p-first
        p-first3
        my-queue
        )
  
   )
)

(displayln "Model2: Pick the smallest line")
(show-marginals (model2)
                (list  "fastest-queue"
                       "val-fastest"
                       "p-first"
                       "p-first3"
                       "my-queue"
                     )
                    #:num-samples 20000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


