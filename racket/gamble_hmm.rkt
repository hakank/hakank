#| 

  Hidden Markov model in Racket.Gamble 

  Port of the WebPPL model http://dippl.org/chapters/04-factorseq.html

  * for the obervations '(#f #f #f)
  var : states
  (#t #f #f #f): 0.8296918550634869
  (#t #t #f #f): 0.09218798389594306
  (#t #f #f #t): 0.03950913595540414
  (#t #f #t #f): 0.016932486838030347
  (#t #t #t #f): 0.010243109321771448
  (#t #t #f #t): 0.004389903995044903
  (#t #f #t #t): 0.004389903995044903
  (#t #t #t #t): 0.002655620935274075

  * for the observation '(#t #t #t)
  var : states
  (#t #t #t #t): 0.9111171840839527
  (#t #t #t #f): 0.043386532575426316
  (#t #t #f #t): 0.0185942282466113
  (#t #f #t #t): 0.0185942282466113
  (#t #t #f #f): 0.004820725841714043
  (#t #f #f #t): 0.0020660253607345884
  (#t #f #t #f): 0.0008854394403148245
  (#t #f #f #f): 0.0005356362046348934

  * for '(#f #f #f #f #f #f #f)
  var : states
  (#t #f #f #f #f #f #f #f): 0.7598810820953769
  (#t #t #f #f #f #f #f #f): 0.08443123134393095
  (#t #f #f #f #f #f #f #t): 0.03618481343311317
  (#t #f #f #t #f #f #f #f): 0.015507777185619959
  (#t #f #t #f #f #f #f #f): 0.015507777185619959
  (#t #f #f #f #t #f #f #f): 0.015507777185619947
  (#t #f #f #f #f #t #f #f): 0.015507777185619947
  (#t #f #f #f #f #f #t #f): 0.015507777185619933
  (#t #t #t #f #f #f #f #f): 0.009381247927103424
  (#t #t #f #f #f #f #f #t): 0.004020534825901471
  (#t #f #f #f #f #f #t #t): 0.004020534825901457
  (#t #t #f #f #f #f #t #f): 0.0017230863539577721
  (#t #t #f #f #f #t #f #f): 0.001723086353957769
  (#t #f #f #f #t #t #f #f): 0.001723086353957769
  ...
  (#t #f #t #t #t #t #f #t): 1.0129843350721756e-6
  (#t #f #t #t #t #t #t #t): 6.127929928214391e-7
  (#t #t #t #f #t #t #t #t): 6.127929928214391e-7
  (#t #t #f #t #t #t #t #t): 6.127929928214391e-7
  (#t #t #t #t #t #t #f #t): 6.127929928214391e-7
  (#t #t #t #t #t #f #t #t): 6.127929928214391e-7
  (#t #t #t #t #f #t #t #t): 6.127929928214391e-7
  (#t #t #t #t #t #t #t #t): 3.7070193392901835e-7


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (transition s) (if s (flip 0.7) (flip 0.3)))
(define (observeState s) (if s (flip 0.9) (flip 0.1)))

; state: (states , observations]
(define (get-states s) (list-ref s 0))
(define (get-obs s) (list-ref s 1))

(define (hmm n)
  (define prev
    (if (= n 1)
    (list (list #t) '()) ; empty state
    (hmm (sub1 n))
    ))

  (define newState (transition (last (get-states prev))))
  (define newObs (list (observeState newState)))
  (list (append (get-states prev) (list newState)) (append (get-obs prev) newObs))
  
  )

; some true observations (the data we observe):
(define trueObs '(#f #f #f))
; (define trueObs '(#t #t #t))
; (define trueObs '(#f #f #f #f #f #f #f))

(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler ; #:transition (slice)

   (define r (hmm (length trueObs)))
   (observe/fail (equal? (get-obs r) trueObs))
   
   (list (get-states r))

   )
  )

(show-marginals (model)
                (list  "states"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.93
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


