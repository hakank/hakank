#| 

  Markov process biased coin in Racket/Gamble 

  From Mathematica DiscreteMarkovProcess
  """
  A biased coin with probability of getting heads being p is flipped n times. 
  Find the probability that the length of the longest run of heads exceeds a given number k. 
  This can be modeled by using a discrete Markov process, where the state i represents 
  having i-1 heads in a run. 
  The resulting transition matrix for p==0.6, n==100, and k==10 is given by

  ...

  ({
  {0.4, 0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  {0.4, 0, 0.6, 0, 0, 0, 0, 0, 0, 0, 0},
  {0.4, 0, 0, 0.6, 0, 0, 0, 0, 0, 0, 0},
  {0.4, 0, 0, 0, 0.6, 0, 0, 0, 0, 0, 0},
  {0.4, 0, 0, 0, 0, 0.6, 0, 0, 0, 0, 0},
  {0.4, 0, 0, 0, 0, 0, 0.6, 0, 0, 0, 0},
  {0.4, 0, 0, 0, 0, 0, 0, 0.6, 0, 0, 0},
  {0.4, 0, 0, 0, 0, 0, 0, 0, 0.6, 0, 0},
  {0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0.6, 0},
  {0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.6},
  {0,   0, 0, 0, 0, 0, 0, 0, 0, 0,   1}
  }

  heads = RandomFunction[DiscreteMarkovProcess[1, m], {0, 200}];

  The probability of getting at least 10 heads in a run after 100 coin flips:
  Probability[x[100] == 11, x in DiscreteMarkovProcess[1, m]]
  ->
  0.20491

  """

  Note: There are some other - and simpler - ways of calculating this:

  (discrete_markov_process_pdf transitions init-state 100 10): 0.20490987499817176

  (probability-of-run-size 100 0.6 10): 0.20490987499817193
  (prob-n-heads-after-k-in-max-m-tosses-cdf 0.6 100 10 100): 0.20490987499817193


  Transitions
  (0.4 0.6 0 0 0 0 0 0 0 0 0)
  (0.4 0 0.6 0 0 0 0 0 0 0 0)
  (0.4 0 0 0.6 0 0 0 0 0 0 0)
  (0.4 0 0 0 0.6 0 0 0 0 0 0)
  (0.4 0 0 0 0 0.6 0 0 0 0 0)
  (0.4 0 0 0 0 0 0.6 0 0 0 0)
  (0.4 0 0 0 0 0 0 0.6 0 0 0)
  (0.4 0 0 0 0 0 0 0 0.6 0 0)
  (0.4 0 0 0 0 0 0 0 0 0.6 0)
  (0.4 0 0 0 0 0 0 0 0 0 0.6)
  (0 0 0 0 0 0 0 0 0 0 1)
  Init-state: (1.0 0 0 0 0 0 0 0 0 0 0)

  Stationary:
  '(1.6120028748187144e-8 9.696058062102922e-9 5.8320951787535635e-9 3.5079548777643768e-9 2.1100045604984235e-9 1.2691495188676667e-9 7.633824738566168e-10 4.5916796463144824e-10 2.7618556485668966e-10 1.6612323182527283e-10 0.9999999597998545)

  (discrete_markov_process_pdf transitions init-state 100 10): 0.20490987499817176

  (probability-of-run-size 100 0.6 10): 0.20490987499817193
  (prob-n-heads-after-k-in-max-m-tosses-cdf 0.6 100 10 100): 0.20490987499817193


  variable : last-val
  0: 0.31714
  10: 0.20332
  1: 0.193
  2: 0.11536
  3: 0.06962
  4: 0.04135
  5: 0.02581
  6: 0.01598
  7: 0.00914
  8: 0.00554
  9: 0.00374
  mean: 3.1980699999999995

  variable : first-pos
  100: 0.8032000000000009
  10: 0.004900000000000005
  21: 0.0033000000000000035
  15: 0.003200000000000003
  18: 0.003200000000000003
  43: 0.003000000000000003
  48: 0.003000000000000003
  14: 0.002900000000000003
  32: 0.002900000000000003
  46: 0.002900000000000003
  95: 0.002900000000000003
  ...
  94: 0.0014000000000000013
  40: 0.0013000000000000012
  58: 0.0013000000000000012
  47: 0.0012000000000000012
  74: 0.0012000000000000012
  63: 0.0011000000000000012
  92: 0.001000000000000001
  mean: 90.70410000000011


  variable : p
  #f: 0.79668
  #t: 0.20332
  mean: 0.20332


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; The band matrix
(define transitions 
  (for/list ([i 11])
    (for/list ([j 11])
      (cond
        [(and (= j 0) (< i 10)) 0.4]
        ((= i (- j 1)) 0.6)
        [(and (= i 10) (= j 10) 1)]
        [else 0]))))

(define init-state '(1.0 0 0 0 0 0 0 0 0 0 0)) ; With 1, enumerate throws "division by zero" (somewhere)

(display-matrix transitions "Transitions")
(show "Init-state" init-state)

(displayln "\nStationary:")
(discrete_markov_process_stationary transitions)
(newline)

(show "(discrete_markov_process_pdf transitions init-state 100 10)" (discrete_markov_process_pdf transitions init-state 100 10))
(newline)
; Some other ways of calculating this
(show "(probability-of-run-size 100 0.6 10)" (probability-of-run-size 100 0.6 10))
(show "(prob-n-heads-after-k-in-max-m-tosses-cdf 0.6 100 10 100)" (prob-n-heads-after-k-in-max-m-tosses-cdf 0.6 100 10 100))

#|
  Simulation with Markov chain
|#
(define (model)
  (; enumerate ; Out of memory
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 100) ; Number of coin tosses
   (define k 10)  ; The state (runs of 10s)
   
   (define x (markov-chain transitions init-state n))
   
   (define last-val (last x))
   (define first-pos (index-of x k)) ; First time we get k
   (define p (= last-val k))
   
   (list last-val
         (if (not first-pos) n first-pos)
         p
         )
   
   )
)

(show-marginals (model)
                (list  "last-val"
                       "first-pos"
                       "p"
                     )
                    #:num-samples 10000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


