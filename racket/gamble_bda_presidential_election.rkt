#| 

  BDA Presidential election  in Racket Gamble.

  https://github.com/probmods/ppaml2016/blob/gh-pages/chapters/5-election.md
  """
  Basic model
  Learning a state-wide preference from poll data
  """

  Sample Election:
  (define : winner
  trump: 0.8704999999999605
  clinton: 0.12950000000000866

  (define : pref
  0.4865599153531963: 0.00010000000000000938
  0.46151370280659404: 0.00010000000000000938
  0.4822595802168018: 0.00010000000000000938
  0.4741672674563471: 0.00010000000000000938
  0.47947990041170185: 0.00010000000000000938
  ...
  0.4736467034084696: 0.00010000000000000938
  0.4582496530520486: 0.00010000000000000938
  0.43499769363753454: 0.00010000000000000938
  0.48205392760299054: 0.00010000000000000938
  0.4645551382382382: 0.00010000000000000938
  mean: 0.47563090750236053
  Credible interval (0.84): 0.44744414340422034..0.5075063882386663


  Simulate result:
  var : pref
  0.0: 0.850900000000005
  1.0: 0.09540000000000055
  1.1102230246251565e-16: 0.00040000000000000235
  0.9999999999999976: 0.00020000000000000118
  0.9999999999999999: 0.00020000000000000118
  ...
  0.9999999023110006: 0.00010000000000000059
  2.1094237467877974e-15: 0.00010000000000000059
  0.07086823160719491: 0.00010000000000000059
  0.6948108736866907: 0.00010000000000000059
  0.9999999999968082: 0.00010000000000000059
  mean: 0.11962603814962416
  Credible interval (0.84): 0.0..0.0

  var : winner
  trump: 0.8806000000000017
  clinton: 0.11940000000000063


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; infer true state preference given poll
(define (trueStatePref)
  (
   importance-sampler
   (define pref (beta 1 1))
   (define counts (make-hash '(("trump" . 304) ("clinton" .  276))))
   (define total (sum (hash-values counts)))
  
   (observe-sample (binomial-dist total pref) (hash-ref counts "clinton"))
   pref
   )
  )

; (writeln (sample (sampler->discrete-dist (trueStatePref) 10000)))


;; Using the learned preference to simulate election-day results

;; simulating general election result
(define (prefDist) (sampler->discrete-dist (trueStatePref) 100))

(define (sampleElection)
  (; enumerate
   importance-sampler
   (define pref (sample (prefDist)))
   (define turnout 2400000)
   (define clintonVotes (binomial turnout pref))
   (define trumpVotes (- turnout clintonVotes))
   (define winner (if (> clintonVotes trumpVotes) "clinton"  "trump"))
   ; (show "winner" winner)
   (list winner
         pref
         )
   )
)

(displayln "Sample Election:")
(show-marginals (sampleElection)
                (list  "winner"
                       "pref"
                      )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(define p0 (+ 1 276))
(define p1 (+ 1 304))
(define (simulateResult)
  (importance-sampler
   (define p (beta p0 p1)) ;; use conjugacy
   
   ; gaussian approximation to binomial because binomial with large n is slow
   (define n 2400000) ; 2012 turnout
   (define np (* n p))
   
   ; use cdf to sample a winner rather than explicitly sampling a number of votes
   (define clintonWinProb (- 1 (dist-cdf (normal-dist np (sqrt (* np (- 1 p)))) (/ n 2)) )
     )
   (define winner (if (flip clintonWinProb) "clinton" "trump"))
   
   (list clintonWinProb
         winner)
   
   )
  )


(displayln "\nSimulate result:")
(show-marginals (simulateResult)
                (list  "pref"
                       "winner"
                       
                      )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
