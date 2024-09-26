#| 

  Coin tosses in Racket Gamble.

  https://edu.swi-prolog.org/mod/assign/view.php?id=254&forceview=1
  """
  Coin tosses

  http://cplint.eu/p/coin_tosses.swinb

  Coin tosses

  Consider a process where you repeatedly toss coins and record the results.

  After each toss, you continue tossing with probability 0.8.

  Write a predicate tosses(Coin,L) that, given an initial coin id Coin, returns in L the list of
  results of coin tosses obtained using the process above.

  Moreover, write a predicate length_tosses(N) that returns the number of coin tosses.

  Compute the probability of the sequence [h,h,h] using MCINTYRE.

  Compute the probability of the sequence [h,h,h,h,h] given that the subsequence [h,h,h] was observed
  using rejection sampling and Metropolis Hastings.

  Compute the probability of the sequences of 10 coin tosses using MCINTYRE.

  Compute the expected length of the sequences of coin tosses using MCINTYRE.
  """ 

  Note: The cplint version (corrected by  Fabrizio Riguzzi) at http://cplint.eu/p/coin_tosses_hakank_rzf.swinb
  give another solutions (note that all are sampled result, i.e. not exact)
   - first problem: probability of the sequence [h,h,h] -> 0.015
   - second problem: probability of the sequence [h,h,h,h,h] given that the subsequence [h,h,h] -> about 0.03
   - third problem: probability of the sequences of 10 coin tosses -> about 0.026
   - fourth problem: expected length of the sequences of coin tosses -> about 4.

  
  Experiment 1 answers the first, third, and fourth problem, whereas experiment 2 answers the
  second problem (since we have to do an observation).

   - first problem: probability of the sequence [h,h,h]: 0.018
   - second problem: probability of the sequence [h,h,h,h,h] given that 
     the subsequence [h,h,h] occurred: 0.0333
   - third problem: probability of the sequences of 10 coin tosses:  0.0267
   - fourth problem: expected length of the sequences of coin tosses: 4.9

  experiment: 1
  var : threeHead
  #f: 0.9821000000000001
  #t: 0.017900000000000006
  mean: 0.017900000000000006

  var : toss-len
  1: 0.20890000000000009
  2: 0.16430000000000006
  3: 0.12460000000000004
  4: 0.09760000000000003
  5: 0.08190000000000003
  ...
  35: 0.00020000000000000006
  31: 0.00010000000000000003
  36: 0.00010000000000000003
  37: 0.00010000000000000003
  43: 0.00010000000000000003
  mean: 4.934600000000002

  var : toss-len >= 10
  #f: 0.9740000000000001
  #t: 0.026000000000000006
  mean: 0.026000000000000006

  experiment: 2
  var : fiveHeadGivenHHH
  #f: 0.9667
  #t: 0.0333
  mean: 0.0333


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (coin-tosses experiment)

  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define coins (vector "head" "tail"))

   (define (toss-a t a)
     (let ([r (categorical-vw2 (vector 0.5 0.5) coins)])
       (if (= t 0)
           (toss-a (add1 t) (append (list r) a))
           (if (flip 0.8)
               (toss-a (add1 t) (append (list r) a))
               a))))

   (define tosses (toss-a 0 '()))
   (define (toss i) (list-ref tosses i))
   
   (define toss-len (length tosses))

   ; Exactly 3 heads
   (define threeHead (and
                       (= toss-len 3)
                       (eq? (list-ref tosses 0) "head")
                       (eq? (list-ref tosses 1) "head")
                       (eq? (list-ref tosses 2) "head")
                       ))

   (when (= experiment 2)
     (observe/fail (and
                    (>= toss-len 3)
                    (eq? (list-ref tosses 0) "head")
                    (eq? (list-ref tosses 1) "head")
                    (eq? (list-ref tosses 2) "head")
                    ))
     )

   ; For experiment 2: given 3 heads, what's the probability of exactly 5 heads
   (define fiveHeadGivenHHH (and
                             (= toss-len 5)
                             (eq? (list-ref tosses 3) "head")
                             (eq? (list-ref tosses 4) "head")))

   (if (= experiment 1)
       (list
        threeHead
        toss-len
        (= toss-len 10)
        )
       (list fiveHeadGivenHHH)
       )
   
   )
  )

(for ([experiment '(1 2)])
  (show "experiment" experiment)
  (show-marginals (coin-tosses experiment)
                  (if (= experiment 1)
                      (list "threeHead"
                            ; "fiveHeadGivenHHH"
                            "toss-len"
                            "toss-len >= 10")
                      (list "fiveHeadGivenHHH")
                        )
                  #:num-samples 10000
                  #:truncate-output 5
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  )
)
