#| 

  Benford distribution in Racket.Gamble 

  https://www.randomservices.org/random/special/Benford.html
  """
  First digit law:
  G1^-1(p) = ceil(b^p-1)
  """

  Also, see Mathematica BenfordLaw:
  ""
  Quantile(BenfordDistribution(base), x)
  -> 
   -1 + Ceiling(base^x)   0 < x < 1
   1                      x <= 0  if 0 <= x <= 1
  -1 + b                  True
  """


  var : b2
  2: 0.9999999999999468
  mean: 1.9999999999998936

  var : b3
  2: 0.6380999999999866
  3: 0.36190000000001665
  mean: 2.361900000000023

  var : b4
  2: 0.5110000000000008
  3: 0.2887000000000245
  4: 0.20030000000002035
  mean: 2.6893000000001566

  var : b5
  2: 0.4370000000000091
  3: 0.24950000000002862
  4: 0.17900000000001692
  5: 0.13450000000000945
  mean: 3.0110000000002186


  This is a port of my WebPPL model benford_dist.wppl.
  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (benford_dist base)
  (let ([u (uniform 0 1)])
    (inexact->exact (ceiling (expt base u)))))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define b2 (benford_dist 2))
   (define b3 (benford_dist 3))
   (define b4 (benford_dist 4))
   (define b5 (benford_dist 5))
   (define b6 (benford_dist 6))
   (define b7 (benford_dist 7))
   (define b8 (benford_dist 8))
   (define b9 (benford_dist 9))
   (define b10 (benford_dist 10))
   (define b27 (benford_dist 27))
   (define b14 (benford_dist 4))
   (list b2
         b3
         b4
         b5
         b6
         b7
         b8
         b9
         b10
         b27
         )
   
   )
)

(show-marginals (model)
                (list "b2"
                      "b3"
                      "b4"
                      "b5"
                      "b6"
                      "b7"
                      "b8"
                      "b9"
                      "b10"
                      "b27"
                     )
                    #:num-samples 10000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


#|
  Can we recover generated data? 
  
  Well, it's not very good (and it's slow).

  data: (3 3 4 3 2 3 3 2 4 2 4 4 2 2 4 3 3 4 2 2)
  var : base
  4: 0.46995
  3: 0.3657
  5: 0.13068
  6: 0.026070000000000003
  7: 0.0076
  mean: 3.8399199999999998

  var : post
  2: 0.54329
  3: 0.30989
  4: 0.12478000000000002
  5: 0.01899
  6: 0.00245
  7: 0.0006
  mean: 2.62922



|#
(define data (for/list ([i 20]) (benford_dist 4)))
(show "data" data)
(define (model2)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define base (+ (random-integer 20) 2))

   (for ([i (length data)])
     ; observe takes too long
     ; (observe-sample (dist-unit (benford_dist base)) (list-ref data i))
     ; (observe/fail (= (benford_dist base) (list-ref data i)))
     ; Simulate WebPPL's factor()
     (observe/fail (<= (abs (- (benford_dist base) (list-ref data i))) 1))
     )

   (define post (benford_dist base))
   
   (list base
         post
         )
   
   )
)

(show-marginals (model2)
                (list "base"
                      "post"
                     )
                    #:num-samples 10000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )
