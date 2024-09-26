#| 

  The generous king in Racket.Gamble 

  TODO: Part 2 is not correct! The WebPPL model (part2) is not
  what is searched for!

  From 
  Gunnar Blom, Lars Holst, Dennis Sandell:
  "Problems and Snapshots from the World of Probability"
  Page 11f, Problem 1.8 The generous king

  A) This is the first part (run1)
     """
     The king of a country with m inhabitants is generous enough to give
     each citizen a gold coin as a Christmas gift. 
     ...
     All m citizen line up in a random order in front of the Royal Palace.
     The king comes out on to a balcony and tosses k gold coins in the air.
     (The probabilities of head/tails is 0.5.)
     The first person in the queue receives as a gift all coins showing
     tails and returns the other to the king. The kind tosses the remaining
     coins; the second person obtains those which come up tails and returns
     the rest, and so on.
     The procedure stops at the m'th toss or earlier if there are no coins 
     left.
     """

  m is the number of citizen. k is the number of coins tossed.
  There are two variants: 


  a) we don't care about if the first m-1 citizens got at least one coin

  Part 1 m: 5  k: 6  all-but-last-got-a-coin: #f
  var : len
  5: 0.32106584310531616
  3: 0.2708168029785157
  4: 0.23013883829116819
  2: 0.16235351562500003
  1: 0.015625000000000007
  mean: 3.6786670088768005

  var : lastCitizen
  0: 0.8265522131696343
  1: 0.1599778477102519
  2: 0.012901439331471906
  3: 0.0005549006164073938
  4: 1.3425014913082131e-5
  5: 1.7322599887847908e-7
  6: 9.313225746154804e-10
  mean: 0.18750000000000006

  This means that it's about 18.8% chance that the last citizen got
  any coin. 
  However, one might to add that all the previous citizens has got at 
  least one coin.


  b) we ensure (observe) that all the first m-1 citizens got at least one coin
  Part 1 m: 5  k: 6  all-but-last-got-a-coin: #t
  var : len
  4: 0.7983193277310925
  5: 0.20168067226890746
  mean: 4.201680672268907

  var : lastCitizen
  0: 0.8960084033613445
  1: 0.10084033613445374
  2: 0.003151260504201678
  mean: 0.1071428571428571

  Then it's about 10.7% chance that the last citizen got a coin.

  Part 1 is a port of my WebPPL model the_generous_king.wppl.

  B) The second part is to select the number of coins (k) so that the last m citizen will 
     receive a gold coin with at least probability 1/2.

     Well, this smells like a loop where we increase the number of
     coins (k) and check the probability that the last citizen has
     a 50% probability of getting a coin.

     Here's a run of 20 experiments with model 2

    We got success for m: 8  k=116
    We got success for m: 8  k=98
    We got success for m: 8  k=77
    We got success for m: 8  k=97
    We got success for m: 8  k=90
    We got success for m: 8  k=98
    We got success for m: 8  k=114
    We got success for m: 8  k=110
    We got success for m: 8  k=107
    We got success for m: 8  k=119
    We got success for m: 8  k=107
    We got success for m: 8  k=92
    We got success for m: 8  k=74
    We got success for m: 8  k=116
    We got success for m: 8  k=96
    We got success for m: 8  k=92
    We got success for m: 8  k=93
    We got success for m: 8  k=129
    We got success for m: 8  k=99
    We got success for m: 8  k=126
   '(116 98 77 97 90 98 114 110 107 119 107 92 74 116 96 92 93 129 99 126)
   (average 205/2 102.5)

  The theoretical value according to the book is about 89
    theoretical for m=8: : 88.722839111673
  so our results is a little to high, but in the same ballpark.



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")



; First part of the problem
; Note: Enumerate is OK for 5 6 but not for much larger values such as 15 16
(define (model1 m k [all-but-last-got-a-coin #f])
  (displayln (format "Part 1 m: ~a  k: ~a  all-but-last-got-a-coin: ~a" m k all-but-last-got-a-coin))
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define (toss a m k)
     (if (or (= m 0) (= k 0))
         a
         (let ((numTails (binomial k 0.5)))
           (toss (append a (list numTails)) (sub1 m) (- k numTails)))))
   
   (define a (toss '() m k))
   (define len (length a))

   (when all-but-last-got-a-coin
     (observe/fail (>= len (sub1 m))) 
     (for/list ([i (sub1 len)]) (observe/fail (>= (list-ref a i) 1)))
     )
   
   ;; How many coins did the last citizen in the queue got?
   (define lastCitizen (if (>= len m) (last a) 0))
   
   (list ; a
         len
         lastCitizen
         )
   
   )
)

(show-marginals (model1 5 6 #f)
                (list  ; "a"
                       "len"
                       "lastCitizen"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(show-marginals (model1 5 6 #t)
                (list  ; "a"
                       "len"
                       "lastCitizen"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


;; Second part:
;;
;; Calculate k (number of coins) such that the last (m'th) citizen has a 50% chance
;; of winning a gold coin.
;; 

;; The expected number of coins (k) for m=8 citizens is
;; (theoreticalProbPart2 8) ~ 88.7, i.e. about 89 coins to toss
(define (theoreticalProbPart2 m)
  (* (log 2) (expt 2 (sub1 m)))
)

(define (model2 m k)
  ; (displayln (format "Part 2 m: ~a k: ~a " m k))
  (; enumerate ; #:limit 1e-01
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; This is the same as in part1.
   (define (toss a m k)
     (if (or (= m 0) (= k 0))
         a
         (let ((numTails (binomial k 0.5)))
           (toss (append a (list numTails)) (sub1 m) (- k numTails)))))

   (define a (toss '() m k))
   (define len (length a))
     
   (define num-got-some-money (for/sum ([i len]) (boolean->integer (>= (list-ref a i) 1))))

   ; Ensure that we didn't finished before m-1
   ; Note that we do not require that all citizen before get any money.
   (observe/fail (>= len (sub1 m)))
   
   ;; How many coins did the last citizen got?
   (define lastCitizen (if (= len m) (last a) 0))

   ; Did the last citizen get any money?
   (define p (>= lastCitizen 1))

   ; Did all citizens get some money?
   ; (define p (= num-got-some-money m))

   (define theoretical (theoreticalProbPart2 m))
   
   ; Return the probability as integer
   (boolean->integer p)
   
   )
)

(displayln "\nModel2 Number of coins (k) to ensure that the last citizen has 50% chance of getting some money")

(define (check m k num-samples num-runs)
  
  (define (loop m k num-samples) 
    (let* ([samples (make-samples (model2 m k) num-samples #:num-internal-samples 1)]
           [num-successes (sum samples)])
      ; Did we get at least 50% success?
      (if (>= num-successes (/ num-samples 2))
          (begin 
            (displayln (format "We got success for m: ~a  k=~a" m k))
            k
            )
          (loop m (add1 k) num-samples)))
    )

  (define lens (repeat (lambda () (loop m 1 num-samples)) num-runs))
  (show "lens" lens)
  (define average (avg lens))
  (show2 "average" average (exact->inexact average))  
  (newline)
  (show2 "theoretical for m: " m ":" (theoreticalProbPart2 m))
  
  )

; Run check for m=8, start with k=1 and use 100 samples each run, and do 10 runs
(check 8 1 100 10)
