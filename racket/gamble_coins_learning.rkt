#| 

  Coins learning in Racket Gamble.

  From srl-pp-tutorial-wasp-stockholm.pdf
  "Statistical Relational learning and Probabilistic Programming"
  by Luc De Raedt, Anton Dries, and Angelika Kimmig
  https://dtai.cs.kuleuven.be/problog/wasp17-tutorial.html
  
  slide 394f:
  """
  - Flipping a coin with unknown weight
  - Prior: uniform distribution on (0,1)
  - Observation: 5x heads in a row
  """

  The ProbLog model return the following which corresponds to the
  density of the probabilist 
  """
   weight(c1,0.1): 3.3994697e-13
   weight(c1,0.3): 2.1679411e-06
   weight(c1,0.5): 0.0041497433
   weight(c1,0.7): 0.1317485 
   weight(c1,0.9): 0.86409959         <----

   weight(c2,0.1): 3.2276726e-06
   weight(c2,0.3): 0.024109997
   weight(c2,0.5): 0.66724754         <----
   weight(c2,0.7): 0.30628626
   weight(c2,0.9): 0.0023529733
  """

  Here we observe 13 tosses and detects the probability of throwing a head (#t).
  The corresponding values of the ProbLog model is in "params ix".

  Note that this model is much simpler than the ProbLog model since ProbLog 
  don't support continous distributions. 
 
  For a model which is much more close to the ProbLog model, see gamble_coins_learning2.rkt


  test: (#t #t #t #t #t #t #t #t #t #t #t #t #t)
  var : p
  mean: 0.9371962607452381

  test: (#t #f #t #t #t #t #t #f #f #t #f #f #t)
  var : p
  mean: 0.5923615189863438

  This is a port of my WebPPL model coins_learning.wppl.
  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model test)

  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler #:transition (slice) ; #:transition (enumerative-gibbs)

   (define p (uniform 0 1))
   (define (data i) (flip p))
   
   (for ([i (length test)])
         (observe-sample (dist-unit (data i)) (list-ref test i))
         )

   (list p)
   
   )
  )


(define test1 '(#t #t #t #t #t #t #t #t #t #t #t #t #t))
(define test2 '(#t #f #t #t #t #t #t #f #f #t #f #f #t))

(for ((test (list test1 test2)))
  (show "test" test)
  (show-marginals (model test)
                  (list "p"
                        )
                  #:num-samples 10000
                  ; #:truncate-output 4
                  #:skip-marginals? #t
                  ; #:credible-interval 0.94
                  ; #:credible-interval2 0.94
                  ; #:show-stats? #t
                  ; #:show-histogram? #t ; 10 ; #t
                  ; #:show-percentiles? #t ; '(0.01 0.2 0.5 0.7 0.99)  ; #t
                  ; #:show-percentiles? '(0.01 0.2 0.5 0.7 0.94 0.99 0.99999)  ; #t                
                  )
  )
