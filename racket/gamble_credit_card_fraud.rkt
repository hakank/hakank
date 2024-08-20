#| 

  Credit card fraud in Racket Gamble.

   From David Heckerman
   "A Tutorial on Learning With Bayesian Networks"
   page 12ff
   """
   To illustrate the process of building a Bayesian network, consider the problem of de-
   tecting credit-card fraud. We begin by determining the variables to model. One possible
   choice of variables for our problem is Fraud(F), Gas(G), Jewelry(J) Age(A) and Sex(S) 
   representing whether or not the current purchase is fraudulent, whether or not there
   was a gas purchase in the last 24 hours, whether or not there was a jewelry purchase in
   the last 24 hours, and the age and sex of the card holder, respectively.
   """

  This model shows some different tests, for example

Test: (eq? fraud #t)
var : fraud
#t: 1.0
mean: 1.0

var : age
age_30_to_50: 0.4
age_above_50: 0.35
age_below_30: 0.25000000000000006

var : sex
male: 0.5
female: 0.5

var : gas
#f: 0.7999999999999999
#t: 0.20000000000000015
mean: 0.20000000000000015

var : jewlery
#f: 0.9500000000000001
#t: 0.05
mean: 0.05


Test: (eq? jewlery #t)
var : fraud
#f: 0.9993468256083812
#t: 0.0006531743916186746
mean: 0.0006531743916186746

var : age
age_30_to_50: 0.6273024152364162
age_above_50: 0.2745591121844654
age_below_30: 0.0981384725791185

var : sex
female: 0.8331156085361271
male: 0.16688439146387282

var : gas
#f: 0.9898758968655924
#t: 0.010124103134407545
mean: 0.010124103134407545

var : jewlery
#t: 0.9999999999999999
mean: 0.9999999999999999

  

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (credit-card-fraud test)

  (display "Test: ")
  (case test
    [(1) (displayln "(eq? age age_below_30)")]
    [(2) (displayln "(eq? age age_30_to_50)")]
    [(3) (displayln "(eq? age age_above_50)")]
    [(4) (displayln "(eq? gas #t)")]
    [(5) (displayln "(eq? sex female)")]
    [(6) (displayln "(eq? jewlery #t)")]
    [(7) (displayln "(eq? fraud #t)")]
    [(8) (displayln "(eq? fraud #f)")]
    )
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define ages  (vector "age_below_30" "age_30_to_50" "age_above_50"))
   (define sexes (vector "male" "female"))
    
   (define fraud (flip 0.00001))
   (define age   (categorical-vw2 (vector 0.25 0.40 0.35) ages))
   (define sex   (categorical-vw2 (vector 0.5 0.5) sexes))
   (define gas   (if fraud
                     (flip 0.2)
                     (flip 0.01)))
    
   (define jewlery
     (cond
       [fraud (flip 0.05)]
       [(and (eq? age "age_below_30") (eq? sex "male"))   (flip 0.0001)]
       [(and (eq? age "age_30_to_50") (eq? sex "male"))   (flip 0.0004)]
       [(and (eq? age "age_above_50") (eq? sex "male"))   (flip 0.0002)]
       [(and (eq? age "age_below_30") (eq? sex "female")) (flip 0.0005)]
       [(and (eq? age "age_30_to_50") (eq? sex "female")) (flip 0.002)]
       [(and (eq? age "age_above_50") (eq? sex "female")) (flip 0.001)]
       [else #f]))
    
    (case test
      [(1) (observe/fail (eq? age "age_below_30"))]
      [(2) (observe/fail (eq? age "age_30_to_50"))]
      [(3) (observe/fail (eq? age "age_above_50"))]
      [(4) (observe/fail (eq? gas #t))]
      [(5) (observe/fail (eq? sex "female"))]
      [(6) (observe/fail (eq? jewlery #t))]
      [(7) (observe/fail (eq? fraud #t))]
      [(8) (observe/fail (eq? fraud #f))]
      )
    
    (list fraud
          age
          sex
          gas
          jewlery
          )

   )
  )

(for ([test (range 1 9)])
  (newline)
  (show-marginals (credit-card-fraud test)
                  (list "fraud"
                        "age"
                        "sex"
                        "gas"
                        "jewlery"
                        )
                  #:num-samples 10000
                  #:truncate-output 5
                  )
)
