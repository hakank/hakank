#| 

   Bar visiting in Racket Gamble.

  I saw this problem in some Swedish Facebook (?) group but 
  cannot find it now.

  The basic problem is this:
   - A visits the bar 3 times during a period, say 10 weeks
   - Each time A visit the bar he sees B.
   What it the chance of this? 
   
  In the Facebook (?) post there was also some information about 
  number of people at the bar etc, but I don't care about that.
   
  What is the probability that this happened? 

  * Model 1
  The model model1 below is based on the following reasoning:
  - The probability that a person visits a bar is a 
    Poisson distribution with some probability
  - A visits the bar exactly 3 times
  - B visits the bar at least 3 times

  (define : n
  mean: 3.0269999999999992
  Credible interval (0.84): 1..5

  (define : p1
  mean: 2.999999999999999
  Credible interval (0.84): 3..3

  (define : p2
  mean: 3.9699999999999984
  Credible interval (0.84): 1..6

  (define : prob1
  mean: 0.7070000000000001
  Credible interval (0.84): 0..1

  (define : prob2
  mean: 0.7070000000000001
  Credible interval (0.84): 0..1

  So the probability that this happens is 68.1% This
  seems to be quite high. 


  * Model 2
    Well, perhaps it's better to make the visits more explicit.
    Say we study this over 30 days instead, and let A and B
    visit the bar with some probability and check if they 
    visit the bar the same day.

    (The period stated in the original problem was over a period
     of 10 weeks, but let's reformulate this to only consider
     3 bardays per week = 30 days.)

    Without any restrictions how many visits B does we get
    quite a high probability: 0.55
    But this is probably since B visits the bar about 16 of the 30 days, which
    is probably not reasonable.

    var : p
    mean: 0.2650000000000002
    Credible interval (0.84): 0..1

    var : p1
    mean: 0.12629738122457762
    Credible interval (0.84): 0.03864805181222142..0.19062622851886407

    var : p2
    mean: 0.548822805374464
    Credible interval (0.84): 0.26365361232231616..0.9826753248344363

   var : sum1
   mean: 3.0000000000000018
   Credible interval (0.84): 3..3

   var : sum2
   mean: 16.493000000000006
   Credible interval (0.84): 5..27

   var : visits same day
   mean: 1.625000000000001
   Credible interval (0.84): 1..3

  * Variant of model 2: Let's instead restrict the number of bar hops for 
    B to atmost 10.Then the probability (p) is much smaller: 0.0079 and the mean number 
    of visits for B is now only about 6 times during these 30 days. And now it's 
    quite probable that A doesn't see B at all: 1-0.63100=0.369

    Added: (observe/fail (<= sum2 10))    

  var : p
  mean: 0.006999999999999995
  Credible interval (0.84): 0..0

  var : p1
  mean: 0.12299281269504801
  Credible interval (0.84): 0.03874894344624974..0.19111168331517886

  var : p2
  mean: 0.22667841117230397
  Credible interval (0.84): 0.09141257881692197..0.37878590956412855

  var : sum1
  mean: 3.0000000000000018
  Credible interval (0.84): 3..3

  var : sum2
  mean: 6.336000000000005
  Credible interval (0.84): 3..9

  var : visits same day
  mean: 0.6310000000000004
  Credible interval (0.84): 0..1


  This is a port of my WebPPL model bar_visiting.wppl 
 
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


; This is a quite simple (simplistic) model.
(define (model1)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice) 
   
   ; (define n (add1 (random-integer 20)))
   (define n 30)
   
   (define p1 (add1 (poisson n)))
   (define p2 (add1 (poisson n)))
   
   (observe/fail (= p1 3))
   
   (define prob1 (and (= p1 3) (>= p2 3)))
   (define prob2 (>= p2 p1))
   
   (list n
         p1
         p2
         prob1
         prob2
         )

   )
  )

#|
(show-marginals (model1)
                (list "n"
                      "p1"
                      "p2"
                      "prob1"
                      "prob2"
                      )
                #:num-samples 1000
                ; #:truncate-output 4
                #:skip-marginals? #t
                #:credible-interval 0.84
                ; #:credible-interval2 0.84                  
                ; #:show-stats? #t
                ; #:show-histogram? #t
                )
|#

;; A little more realistic approach
;; Here we study what happens each day for a couple of days, say 30.
(define (model2)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice) 
   
   (define n 30) ;; number of days

   ;; Probabilities of visiing a bar at each day
   (define p1 (beta 1 1))
   (define p2 (beta 1 1))

   ;; Person p (1 or 2) visits the bar at day day
   (defmem (visits p day)
     (if (= p 1)
         (flip p1)
         (flip p2)
         ))

   ;; How many visiting days?
   (define sum1 (for/sum ([d n]) (if (visits 1 d) 1 0)))
   (define sum2 (for/sum ([d n]) (if (visits 2 d) 1 0)))

   ;; Visited the same day
   (define visits_same_day
     ; = sum( mapN(function(i) ( return visits(1,i) && visits(2,i) ? 1 : 0),n))
     (for/sum ([d n])
       (if (and (visits 1 d) (visits 2 d)) 1 0)))

   ;; What is the probability that person A see person B during all his 3 visits.
   (define p (= visits_same_day 3))

   ;; Person 1 visits exactly 3 times
   (observe/fail (= sum1 3))
   ;; Person 2 visits at least 3 times
   (observe/fail (>= sum2 3))


   ;; Restrict the number of visits for person 2, to - say - atmost 10 visits
   (observe/fail (<= sum2 10))

   ;; Now assume that A actually saw B each of A's visit.
   ; (observe/fail (eq? p #t))

   (list p
         p1
         p2
         sum1
         sum2
         visits_same_day
         )

   )
  )

(show-marginals (model2)
                (list "p"
                      "p1"
                      "p2"
                      "sum1"
                      "sum2"
                      "visits same day"
                      )
                #:num-samples 1000
                ; #:truncate-output 4
                #:skip-marginals? #t
                #:credible-interval 0.84
                ; #:credible-interval2 0.84                  
                ; #:show-stats? #t
                ; #:show-histogram? #t
                )
