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

  * Observing  (#t #t #t #t #t #t #t #t #t #t #t #t #t)
    i.e. 13 heads

    var : params ix
    0.9: 0.8640995866847063
    0.7: 0.13174850208677874
    0.5: 0.00414974328708235
    0.3: 2.1679410927013e-6
    0.1: 3.3994697007778543e-13
    mean: 0.8719891015028838

  * Observing (#t #f #t #t #t #t #t #f #f #t #f #f #t) 

    var : params ix
    0.5: 0.66724754369546
    0.7: 0.3062862583024128
    0.3: 0.02410999700922786
    0.9: 0.0023529733203061774
    0.1: 3.2276725930126056e-6
    mean: 0.5573751505177221

 
  This is a port of my WebPPL model coins_learning2.wppl.
  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model test)

  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler ; #:transition (slice) ; #:transition (enumerative-gibbs)

   (define params '(0.1 0.3 0.5 0.7 0.9))
   (define params_prior (vector 0.05 0.2 0.5 0.2 0.05))
   (define len (length params))

   ;; Generate the index for param/weight interval
   ;; according to the priors in params_prior
   (define ix (categorical-vw2 params_prior (vector 0 1 2 3 4)))
   
   ;; generate the param value to use for this data point
   (defmem (data i) (flip (list-ref params ix)))

   (for ([i (length test)])
     (observe/fail (eq? (data i) (list-ref test i)))
     )
   ;; (observe/fail (eq? (data 0) #t))
   ;; (observe/fail (eq? (data 1) #t))
   ;; (observe/fail (eq? (data 2) #t))
   ;; (observe/fail (eq? (data 3) #t))
   ;; (observe/fail (eq? (data 4) #t))
   ;; (observe/fail (eq? (data 5) #t))
   ;; (observe/fail (eq? (data 6) #t))
   ;; (observe/fail (eq? (data 7) #t))
   ;; (observe/fail (eq? (data 8) #t))
   ;; (observe/fail (eq? (data 9) #t))
   ;; (observe/fail (eq? (data 10) #t))
   ;; (observe/fail (eq? (data 11) #t))
   ;; (observe/fail (eq? (data 12) #t))


   (list ix
         (= ix 0)
         (= ix 1)
         (= ix 2)
         (= ix 3)
         (= ix 4)
         (list-ref params ix)
         )

   )
  )


(define test1 '(#t #t #t #t #t #t #t #t #t #t #t #t #t))
(define test2 '(#t #f #t #t #t #t #t #f #f #t #f #f #t))

(for ([test (list test1 test2)])
  (show "test" test)
  (show-marginals (model test)
                  (list "ix"
                        "ix = 0"
                        "ix = 1"
                        "ix = 2"
                        "ix = 3"
                        "ix = 4"
                        "params ix"
                        )
                  #:num-samples 1000
                  ; #:truncate-output 4
                  ; #:skip-marginals? #t
                  ; #:credible-interval 0.94
                  ; #:credible-interval2 0.94
                  ; #:show-stats? #t
                  ; #:show-histogram? #t ; 10 ; #t
                  ; #:show-percentiles? #t ; '(0.01 0.2 0.5 0.7 0.99)  ; #t
                  ; #:show-percentiles? '(0.01 0.2 0.5 0.7 0.94 0.99 0.99999)  ; #t                
                  )
  )
