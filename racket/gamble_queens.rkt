#| 

  N-queens in Racket.Gamble 

  This model use the standard constraint programming 
  formulation of the n-queens problem. 
  Cf http://hakank.org/picat/queens.pi

  This is a port of my WebPPL model queens.wppl
  but this Gamble model is much faster.
  

  The fastest is model3 which uses permutation-dist and all_different
  and finds all solutions (using importance-sampler)
  
  N  NumSols  Time 
  ----------------
  4   2       0.093s
  5  10       0.098s
  6   4       1.393s
  7  40       0.999s
  8  92       3.975s

  model1 and model2 use enumerate and is quite slower.
  Compare with my Picat model http://hakank.org/picat/queens.pi 
  which solves n=8 in 0s, and n=14 in 4.2s. 


  Warning: using probabilistic programming for this problem is 
  not the best choice. :-)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (all_different a n)
  (for ([i n])
    (for ([j i]) 
      (when (not (= i j))
        (observe/fail (not (= (list-ref a i) (list-ref a j)))))))
  )
                

#|

  * Using enumerate

  (n 4)
  var : queens
  (1 3 0 2): 1/2 (0.5)
  (2 0 3 1): 1/2 (0.5)

  cpu time: 2 real time: 2 gc time: 0

  (n 5)
  var : queens
  (2 0 3 1 4): 1/10 (0.1)
  (4 2 0 3 1): 1/10 (0.1)
  (1 3 0 2 4): 1/10 (0.1)
  (1 4 2 0 3): 1/10 (0.1)
  (3 1 4 2 0): 1/10 (0.1)
  (4 1 3 0 2): 1/10 (0.1)
  (3 0 2 4 1): 1/10 (0.1)
  (0 3 1 4 2): 1/10 (0.1)
  (0 2 4 1 3): 1/10 (0.1)
  (2 4 1 3 0): 1/10 (0.1)

  cpu time: 29 real time: 29 gc time: 2

  (n 6)
  var : queens
  (1 3 5 0 2 4): 1/4 (0.25)
  (3 0 4 1 5 2): 1/4 (0.25)
  (2 5 1 4 0 3): 1/4 (0.25)
  (4 2 0 5 3 1): 1/4 (0.25)

  cpu time: 475 real time: 476 gc time: 30

  (n 7)
  var : queens
  (0 2 4 6 1 3 5): 1/40 (0.025)
  (3 6 4 1 5 0 2): 1/40 (0.025)
  (4 6 1 3 5 0 2): 1/40 (0.025)
  (2 6 3 0 4 1 5): 1/40 (0.025)
  (2 6 1 3 5 0 4): 1/40 (0.025)
  (0 5 3 1 6 4 2): 1/40 (0.025)
  (5 0 2 4 6 1 3): 1/40 (0.025)
  (3 0 2 5 1 6 4): 1/40 (0.025)
  (2 0 5 3 1 6 4): 1/40 (0.025)
  (5 3 1 6 4 2 0): 1/40 (0.025)
  (4 2 0 5 3 1 6): 1/40 (0.025)
  (3 0 4 1 5 2 6): 1/40 (0.025)
  (6 3 0 4 1 5 2): 1/40 (0.025)
  (2 5 1 4 0 3 6): 1/40 (0.025)
  (1 6 4 2 0 5 3): 1/40 (0.025)
  (0 3 6 2 5 1 4): 1/40 (0.025)
  (1 3 5 0 2 4 6): 1/40 (0.025)
  (1 4 0 3 6 2 5): 1/40 (0.025)
  (5 2 4 6 0 3 1): 1/40 (0.025)
  (2 4 6 1 3 5 0): 1/40 (0.025)
  (4 0 3 6 2 5 1): 1/40 (0.025)
  (1 5 2 6 3 0 4): 1/40 (0.025)
  (4 6 1 5 2 0 3): 1/40 (0.025)
  (4 0 5 3 1 6 2): 1/40 (0.025)
  (6 2 5 1 4 0 3): 1/40 (0.025)
  (1 4 2 0 6 3 5): 1/40 (0.025)
  (3 6 2 5 1 4 0): 1/40 (0.025)
  (0 4 1 5 2 6 3): 1/40 (0.025)
  (1 4 6 3 0 2 5): 1/40 (0.025)
  (6 4 2 0 5 3 1): 1/40 (0.025)
  (5 2 0 3 6 4 1): 1/40 (0.025)
  (2 0 5 1 4 6 3): 1/40 (0.025)
  (1 3 0 6 4 2 5): 1/40 (0.025)
  (3 5 0 2 4 6 1): 1/40 (0.025)
  (3 1 6 4 2 0 5): 1/40 (0.025)
  (5 1 4 0 3 6 2): 1/40 (0.025)
  (5 2 6 3 0 4 1): 1/40 (0.025)
  (4 1 5 2 6 3 0): 1/40 (0.025)
  (6 1 3 5 0 2 4): 1/40 (0.025)
  (5 3 6 0 2 4 1): 1/40 (0.025)

  cpu time: 9752 real time: 9767 gc time: 1054

  (n 8)
  var : queens
  (1 7 5 0 2 4 6 3): 1/92 (0.010869565217391304)
  (3 6 2 7 1 4 0 5): 1/92 (0.010869565217391304)
  (5 2 6 1 3 7 0 4): 1/92 (0.010869565217391304)
  (2 0 6 4 7 1 3 5): 1/92 (0.010869565217391304)
  ...
  (4 0 3 5 7 1 6 2): 1/92 (0.010869565217391304)
  (2 5 7 0 4 6 1 3): 1/92 (0.010869565217391304)
  (6 1 3 0 7 4 2 5): 1/92 (0.010869565217391304)
  (5 3 6 0 7 1 4 2): 1/92 (0.010869565217391304)

  cpu time: 229491 real time: 229578 gc time: 37152

  * With importance-sampler instead: Much slower

  (n 4)
  var : queens
  (2 0 3 1): 0.509
  (1 3 0 2): 0.491

  cpu time: 2006 real time: 2006 gc time: 167

  (n 5)
  var : queens
  (0 2 4 1 3): 0.11300000000000002
  ...
  cpu time: 6296 real time: 6297 gc time: 22

  (n 6)
  var : queens
  (3 0 4 1 5 2): 0.281
  (1 3 5 0 2 4): 0.244
  (4 2 0 5 3 1): 0.244
  (2 5 1 4 0 3): 0.231

  cpu time: 291849 real time: 292161 gc time: 1344

  (n 7)
  var : queens
  (1 6 4 2 0 5 3): 0.03499999999999999
  ...
  cpu time: 616026 real time: 616485 gc time: 2641

  (n 8)

  Too slöw!

|#
(define (model [n 4])
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (defmem (queen i) (random-integer n))
   
   (define queens (for/list ([i n]) (queen i)))
   
   ;; all_different(queen,n);   
   (for ([i n])
     (for ([j i])
       (observe/fail (and
                      (not (= (queen i) (queen j)))
                      (not (= (+ (queen i) i) (+ (queen j) j)))
                      (not (= (- (queen i) i) (- (queen j) j)))
                      )
                     )
       )
     )
   
   (list queens
         )
   
   )
)

#|
(for ([n (range 4 9)])
  (newline)
  (show2 "n" n)
  (time (show-marginals (model n)
                  (list  "queens"
                         )
                  #:num-samples 1000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ))
)
|#


#|
  This model uses all_different instead.

  In constraint programming, this tends to me (much) faster 
  than the approach in model1, and perhaps it is _slightly_
  faster here as well.

  (n 4)
  var : queens
  (1 3 0 2): 1/2 (0.5)
  (2 0 3 1): 1/2 (0.5)

  cpu time: 2 real time: 2 gc time: 0

  (n 5)
  var : queens
  (2 0 3 1 4): 1/10 (0.1)
  (4 2 0 3 1): 1/10 (0.1)
  (1 3 0 2 4): 1/10 (0.1)
  (1 4 2 0 3): 1/10 (0.1)
  (3 1 4 2 0): 1/10 (0.1)
  (4 1 3 0 2): 1/10 (0.1)
  (3 0 2 4 1): 1/10 (0.1)
  (0 3 1 4 2): 1/10 (0.1)
  (0 2 4 1 3): 1/10 (0.1)
  (2 4 1 3 0): 1/10 (0.1)

  cpu time: 29 real time: 29 gc time: 1

  (n 6)
  var : queens
  (1 3 5 0 2 4): 1/4 (0.25)
  (3 0 4 1 5 2): 1/4 (0.25)
  (2 5 1 4 0 3): 1/4 (0.25)
  (4 2 0 5 3 1): 1/4 (0.25)

  cpu time: 461 real time: 462 gc time: 27

  (n 7)
  var : queens
  (0 2 4 6 1 3 5): 1/40 (0.025)
  (3 6 4 1 5 0 2): 1/40 (0.025)
  (4 6 1 3 5 0 2): 1/40 (0.025)
  ...
  (4 1 5 2 6 3 0): 1/40 (0.025)
  (6 1 3 5 0 2 4): 1/40 (0.025)
  (5 3 6 0 2 4 1): 1/40 (0.025)

  cpu time: 9082 real time: 9091 gc time: 806

  (n 8)
  var : queens
  (1 7 5 0 2 4 6 3): 1/92 (0.010869565217391304)
  (3 6 2 7 1 4 0 5): 1/92 (0.010869565217391304)
  (5 2 6 1 3 7 0 4): 1/92 (0.010869565217391304)
  ...
  (2 5 7 0 4 6 1 3): 1/92 (0.010869565217391304)
  (6 1 3 0 7 4 2 5): 1/92 (0.010869565217391304)
  (5 3 6 0 7 1 4 2): 1/92 (0.010869565217391304)

  cpu time: 226178 real time: 227255 gc time: 37942

 
  * Using importance-sampler is a littler faster.
    All solutions are found.

  Showing just the times.
  (n 4)
  var : queens
  (1 3 0 2): 1/2 (0.5)
  ...
  cpu time: 2 real time: 2 gc time: 0

  (n 5)
  var : queens
  (2 0 3 1 4): 1/10 (0.1)
  ...
  cpu time: 177 real time: 178 gc time: 150

  (n 6)
  var : queens
  (1 3 5 0 2 4): 1/4 (0.25)
  ...
  cpu time: 441 real time: 442 gc time: 32

  (n 7)
  var : queens
  (0 2 4 6 1 3 5): 1/40 (0.025)
  ...
  cpu time: 8933 real time: 8946 gc time: 984

  (n 8)
  var : queens
  (1 7 5 0 2 4 6 3): 1/92 (0.010869565217391304)
  ...
  cpu time: 213996 real time: 214238 gc time: 36933


|#
(define (model2 [n 4])
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (defmem (queen i) (random-integer n))
   
   (define queens (for/list ([i n]) (queen i)))
   
   (all_different queens n)
   (all_different (for/list ([i n]) (+ (queen i) i)  n))
   (all_different (for/list ([i n]) (- (queen i) i)  n))
  
   
   (list queens
         )
   
   )
)

#|
(for ([n (range 4 9)])
  (newline)
  (show2 "n" n)
  (time (show-marginals (model n)
                  (list  "queens"
                         )
                  #:num-samples 1000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ))
)
|#


#|
  Using permutation-dist and all_different, using importance-sampler 1000 samples.
  
  This is much faster!

  (n 4)
  var : queens
  #(2 0 3 1): 0.519
  #(1 3 0 2): 0.481

  cpu time: 93 real time: 93 gc time: 14

  (n 5)
  var : queens
  #(0 2 4 1 3): 0.11800000000000001
  ...
  cpu time: 98 real time: 98 gc time: 2

  (n 6)
  var : queens
  #(2 5 1 4 0 3): 0.258
  #(1 3 5 0 2 4): 0.258
  #(4 2 0 5 3 1): 0.249
  #(3 0 4 1 5 2): 0.235

  cpu time: 1393 real time: 1396 gc time: 12

  (n 7)
  var : queens
  #(0 2 4 6 1 3 5): 0.037999999999999985
  ... 
  cpu time: 999 real time: 1000 gc time: 8

  (n 8)
  var : queens
  #(3 1 4 7 5 0 2 6): 0.021999999999999985
  #(4 0 7 3 1 6 2 5): 0.019999999999999987
  ...
  cpu time: 3975 real time: 3980 gc time: 36


  For n=9 this models finds 327 solutions (of 352) in 9.6s (1000 samples)
  For n=10 this model finds 549 solutions (of 724) in 53.4s (1000 samples)


|#
(define (model3 [n 4])
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

 
   (define queens (sample (permutation-dist n)))
   (define (queen i) (vector-ref queens i))
   
   ; (all_different queens n) ; Not needed since queens is a permutation
   (all_different (for/list ([i n]) (+ (queen i) i))  n)
   (all_different (for/list ([i n]) (- (queen i) i))  n)
  
   (list queens
         )
   
   )
)

(for ([n (range 4 9)])
  (newline)
  (show2 "n" n)
  (time (show-marginals (model3 n)
                  (list  "queens"
                         )
                  #:num-samples 1000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ))
)
