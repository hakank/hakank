#| 

  Waiting for a truck in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/1005
  """
  On a given highway, trucks arrive at the station according to 
  a Poisson process with Lambda = 0.1/minute. This means that after 
  a truck is just passed, the time for the next truck to arrive is an 
  exponential random number with average arrival time of 10 minutes. 
  Your car just broke on this highway, and you are waiting for the next 
  truck for hitchhiking, what is your expected waiting time? On average 
  how many minutes ago the last truck left?

  Solution: Using memoryless property of exponential distribution, the 
            expected waiting time is 10 minutes. This also holds backwards, 
            hence the expected time last truck passed is also 10 minutes. 
            But this does not violate the total inter-arrival time of 10 
            minutes, because if your car breaks at a random time, you are 
            more likely to be in long interval than a short one.
  """

  Using enumerate #:limit 1e-10

var : t1
9: 0.12511003573194632
10: 0.12511003573194626
11: 0.11373639611995118
8: 0.1125990321587517
12: 0.09478033009995933
...
33: 5.224512855652103e-9
34: 1.53455680744031e-9
35: 4.349877959542192e-10
36: 1.1748325429464157e-10
37: 2.422174568530283e-11
mean: 9.99999999849171

var : t2
20: 0.08883531740079714
19: 0.08883531740079713
21: 0.08460506419123537
18: 0.08439355153075728
22: 0.07691369471930488
...
0: 2.061153622640693e-9
52: 1.1471922343361567e-9
53: 4.287878339337755e-10
54: 1.5437685505177113e-10
55: 4.813694533085184e-11
mean: 19.999999996983423

var : t1, t2
(9 18): 0.01565252103967827
(10 19): 0.015652521039678256
(9 19): 0.015652521039678256
(10 20): 0.015652521039678256
(11 20): 0.014229564581525701
...
(13 50): 2.404878681056132e-12
(33 34): 2.3736936884300166e-12
(1 34): 2.373693688430008e-12
(36 40): 2.308681225130343e-12
(4 40): 2.308681225130343e-12

var : diff
9: 0.12511003573194632
10: 0.12511003573194626
11: 0.11373639611995119
8: 0.1125990321587517
12: 0.09478033009995933
...
33: 5.2245128556521e-9
34: 1.534556807440309e-9
35: 4.34987795954219e-10
36: 1.174832542946419e-10
37: 2.4221745685302808e-11
mean: 9.99999999849171


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-10
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define lambda_ (/ 1 0.1))
   (define t1 (poisson lambda_))
   (define t2 (+ t1 (poisson lambda_)))
   (define diff (abs (- t1 t2)))

   (list t1
         t2
         (list t1 t2)
         diff)

   )
)

(show-marginals (model)
                (list  "t1"
                       "t2"
                       "t1, t2"
                       "diff"
                       )
                #:num-samples 1000000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


