#| 

  Papers under 9 hours in Racket/Gamble 

  From Pascal Bercker: Five-Minute Probability Problem — Netica vs. Python
  https://medium.com/@pbercker/five-minute-probability-problem-with-netica-d05299e454d5
  """
  I saw this simple little problem here 
  [https://www.youtube.com/watch?v=slbZ-SLpIgg ]
  using a Monte Carlo Simulation with Python. We will redo this with a 
  simple Bayesian Belief Network (Netica) [https://www.norsys.com/netica.html].

  Someone is tasked with writing two reports under a deadline. One report 
  could be anywhere from 1 to 5 hours, and the other anywhere from 2 to 6 
  hours. What is the probability you can get both done under 9 hours? 
  We want a probability distribution over the estimated time to completion. 

  """
  
  Using uniform discrete distribution:

  variable : p
  #t: 22/25 (0.88)
  #f: 3/25 (0.12)
  mean: 22/25 (0.88)

  variable : total
  7: 1/5 (0.2)
  6: 4/25 (0.16)
  8: 4/25 (0.16)
  5: 3/25 (0.12)
  9: 3/25 (0.12)
  4: 2/25 (0.08)
  10: 2/25 (0.08)
  3: 1/25 (0.04)
  11: 1/25 (0.04)
  mean: 7 (7.0)

  variable : paper1
  1: 1/5 (0.2)
  2: 1/5 (0.2)
  3: 1/5 (0.2)
  4: 1/5 (0.2)
  5: 1/5 (0.2)
  mean: 3 (3.0)

  variable : paper2
  2: 1/5 (0.2)
  3: 1/5 (0.2)
  4: 1/5 (0.2)
  5: 1/5 (0.2)
  6: 1/5 (0.2)
  mean: 4 (4.0)


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
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define paper1 (+ 1 (random-integer 5)))
   (define paper2 (+ 2 (random-integer 5)))
   (define total (+ paper1 paper2))

   (define p (<= total 9))
   
   (list p total paper1 paper2 )
   

   )
)

(show-marginals (model)
                (list  "p"
                       "total"
                       "paper1"
                       "paper2"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.5 0.84 0.90 0.95 0.99)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

#|

  Uniform distribution

variable : p
#t: 0.8778999999999597
#f: 0.12210000000000781
mean: 0.8778999999999597

variable : total
7.687662758639514: 0.00010000000000000938
5.955113190846411: 0.00010000000000000938
3.893002721887214: 0.00010000000000000938
8.659694412075085: 0.00010000000000000938
5.650109080416776: 0.00010000000000000938
...
8.127677228897081: 0.00010000000000000938
9.057752124036767: 0.00010000000000000938
6.241180060004223: 0.00010000000000000938
6.473694861523931: 0.00010000000000000938
6.52276371622804: 0.00010000000000000938
mean: 6.99566121163217
HPD interval (0.84): 4.532993040714076..9.299678724802373

variable : paper1
3.1447558072649895: 0.00010000000000000938
4.1436436432129415: 0.00010000000000000938
3.107856614150614: 0.00010000000000000938
4.7910059756900285: 0.00010000000000000938
3.9702854849908924: 0.00010000000000000938
...
1.9447051753519746: 0.00010000000000000938
4.522550290611214: 0.00010000000000000938
1.213653240455285: 0.00010000000000000938
1.1346567086895452: 0.00010000000000000938
4.096693718832055: 0.00010000000000000938
mean: 3.0024575848270167
HPD interval (0.84): 1.2978113205043493..4.656322376922485

variable : paper2
3.5279046580670794: 0.00010000000000000938
3.738891939094645: 0.00010000000000000938
3.0367377017721164: 0.00010000000000000938
5.179708364740792: 0.00010000000000000938
5.926898370681997: 0.00010000000000000938
...
4.379155235100605: 0.00010000000000000938
5.957401015595396: 0.00010000000000000938
5.918641445943486: 0.00010000000000000938
2.395832456260256: 0.00010000000000000938
3.522686254400467: 0.00010000000000000938
mean: 3.993203626805179
HPD interval (0.84): 2.446911854892426..5.7866871365418024


|#

(displayln "\nModel2\n")
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define paper1 (uniform 1 5))
   (define paper2 (uniform 2 6))
   (define total (+ paper1 paper2))

   (define p (<= total 9))
  
   (list p total paper1 paper2 )
   

   )
)

(show-marginals (model2)
                (list  "p"
                       "total"
                       "paper1"
                       "paper2"
                       )
                #:num-samples 10000
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

