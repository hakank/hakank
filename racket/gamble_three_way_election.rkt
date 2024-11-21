#| 

  Three way election in Racket/Gamble 

  From Statistics101 (Resampling Stats)
  File threewayElection.txt
  """
  A poll was taken before an election in which three politicians were
  competing. The sample size was 400. The result was that 10 percent of
  those polled favored politician A, 40 percent favored politician B,
  and the rest favored politician C. What are the 95 percent confidence
  intervals for the results for each politician?
  -> 
  polAConfidence: (7.249999999999999 13.0)
  polBConfidence: (35.25 44.75)
  polCConfidence: (45.25 54.75)
  """

  Two models below:
  - model: resampling with importance-sampler
  - model2: multinomial with enumerate


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

#|
  Resampling

  variable : a
  mean: 10.001749999999998
  HPD interval (0.95): 7.000000000000001..12.75

  variable : b
  mean: 40.01249999999998
  HPD interval (0.95): 35.25..44.5

  variable : c
  mean: 49.98574999999997
  HPD interval (0.95): 44.5..54.25

  variable : a > 50
  mean: 0 (0.0)

  variable : b > 50
  mean: 0 (0.0)

  variable : c > 50
  mean: 0.4900000000000004


|#
(define population (flatten (append (rep 10 "A") (rep 40 "B") (rep 50 "C"))))
(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 400) ; sample size
   (define sample (resample n population))
   (define a (* (/ (count-occurrences-eq "A" sample) n) 100.0))
   (define b (* (/ (count-occurrences-eq "B" sample) n) 100.0))
   (define c (* (/ (count-occurrences-eq "C" sample) n) 100.0))

   (list a
         b
         c
         (> a 50)
         (> b 50)
         (> c 50)
    )
    
   )
)

(show-marginals (model1)
                (list  "a"
                       "b"
                       "c"
                       "a > 50"
                       "b > 50"
                       "c > 50"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.95)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
  Using multinomial (enumerate)

  variable : a
  mean: 10.000000000000133
  HPD interval (0.95): 7.000000000000001..12.75

  variable : b
  mean: 40.00000000000054
  HPD interval (0.95): 34.75..44.25

  variable : c
  mean: 50.00000000000064
  HPD interval (0.95): 45.25..54.75

  variable : a > 50
  mean: 9.015968215178488e-92

  variable : b > 50
  mean: 2.123787760511589e-5

  variable : c > 50
  mean: 0.48006534901810877


|#


(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 400)
   (define the-sample (multinomial_dist n '(10/100 40/100 50/100)))
   (define a (* 100.0 (/ (first the-sample) n)))
   (define b (* 100.0 (/ (second the-sample) n)))
   (define c (* 100.0 (/ (third the-sample) n)))
   
   (list a
         b
         c
         (> a 50)
         (> b 50)
         (> c 50)
         )
   )
)

(show-marginals (model2)
                (list  "a"
                       "b"
                       "c"
                       "a > 50"
                       "b > 50"
                       "c > 50"
                       )
                #:skip-marginals? #t
                #:hpd-interval (list 0.95)
                )

