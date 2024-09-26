#| 

  Cheating in Racket Gamble.

  From 
  https://discourse.julialang.org/t/fitting-a-observed-value-to-a-binomial-distribution-turing/66619
  """
  Fitting a observed value to a Binomial Distribution Turing

  I am new to Turing and trying to learn it by trying to replicate the Chapters from the book 
  https://github.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers 2 .

  Here is my problem (from Chapter 2) :

  1.) Have a probability p ~ Uniform(0,1) which I have to infer the “number_of_cheaters”
  2.) p_skewed = p*(0.5) + (0.25) (deterministic value)
  3.) with model:
  yes_responses = pm.Binomial(“number_of_cheaters”, 100, p_skewed, observed=35)

  How do I write this in Turing ?
  """

var : p
0.19925904214519097: 0.004485194016958819
0.19914036160828436: 0.004485147178906518
0.20106000053241854: 0.004485052592399573
0.19880502865450597: 0.004484977276292502
0.1986090001442172: 0.00448485225106759
...
0.9916912888343885: 4.406556329269343e-18
0.9920819961356594: 4.230324328320619e-18
0.9937895493833876: 3.537500992783819e-18
0.9953140378988629: 3.013415560701735e-18
0.998651959635226: 2.1165496412082167e-18
mean: 0.2059051528124581

var : p_skewed
0.3496295210725955: 0.004485194016958819
0.34957018080414215: 0.004485147178906518
0.3505300002662093: 0.004485052592399573
0.349402514327253: 0.004484977276292502
0.3493045000721086: 0.00448485225106759
...
0.7458456444171943: 4.406556329269343e-18
0.7460409980678298: 4.230324328320619e-18
0.7468947746916939: 3.537500992783819e-18
0.7476570189494314: 3.013415560701735e-18
0.7493259798176131: 2.1165496412082167e-18
mean: 0.35295257640622946


  Cf gamble_cheating0.rkt which shows about the same result.

  This is a port of my WebPPL model cheating2.wppl
  (which is a port of an adaped Turing.jl model I wrote as the answer).

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (enumerative-gibbs)

   (define yes_responses 35)
   (define N 100)

   (define p (uniform 0 1))
   (define p_skewed (+ (* p 0.5) 0.25))
   
   (observe-sample (binomial-dist N p_skewed) yes_responses)  ;; Observe the yes_responses

   (list p
         p_skewed
    )

   )
  )

(show-marginals (model)
                (list "p"
                      "p_skewed"
                      )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t                
                )
