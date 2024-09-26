#| 

  How many times was the coin tossed? in Racket.Gamble 

  From https://medium.com/illumination/how-many-times-was-the-coin-tossed-acf166b61eeb
  """
  An unfair coin lands on Head with 1/4. When tossed n times, the probability
  of 2 heads == probabillity of 3 heads. 
  What is n?
  """

  It's 0, 1 or 11.

  * Without any lower bound on n

  For n=0 and n=1, then p2 = p3 = 0.
  For n=11, then p2 = p3 ~ 0.258103609085083

  var : n
  0: 1/3 (0.3333333333333333)
  1: 1/3 (0.3333333333333333)
  11: 1/3 (0.3333333333333333)
  mean: 4 (4.0)

  var : p2
  0.0: 2/3 (0.6666666666666666)
  0.258103609085083: 1/3 (0.3333333333333333)
  mean: 0.08603453636169434

  var : p3
  0.0: 2/3 (0.6666666666666666)
  0.258103609085083: 1/3 (0.3333333333333333)
  mean: 0.08603453636169434

  var : n p3 p3
  (11 0.258103609085083 0.258103609085083): 1/3 (0.3333333333333333)
  (1 0.0 0.0): 1/3 (0.3333333333333333)
  (0 0.0 0.0): 1/3 (0.3333333333333333)

  * n >= 3

  However, it probably make more sense to restrict n to be >= 3 since there 
  have been observed 3 heads.

  var : n
  11: 1 (1.0)
  mean: 11 (11.0)

  var : p2
  0.258103609085083: 1 (1.0)
  mean: 0.258103609085083

  var : p3
  0.258103609085083: 1 (1.0)
  mean: 0.258103609085083

  var : n p3 p3
  (11 0.258103609085083 0.258103609085083): 1 (1.0)


  See model2 below for another approach, using discrete-dist-weight.
)

  Mathematica:
  """
  Reduce[{
  PDF[BinomialDistribution[n, 1/4], 2] == 
   PDF[BinomialDistribution[n, 1/4], 3], n > 0}, {x}, Integers]
  -> 
  x \[Element] Integers && (n == 1 || n == 11)

  PDF[BinomialDistribution[11, 1/4], 2] // N
  -> 
  0.258104
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n (random-integer 20))
   (observe/fail (>= n 3))
   
   (define p 1/4)

   (define p2 (dist-pdf (binomial-dist n p) 2))
   (define p3 (dist-pdf (binomial-dist n p) 3))
   (observe-sample (dist-unit p2) p3)
   (observe/fail (= p2 p3))

   (list n
         p2
         p3
         (list n p2 p3)
         )
   

   )
)

#|
(show-marginals (model)
                (list  "n"
                       "p2"
                       "p3"
                       "n p3 p3"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
|#


#|
  Another approach using discrete-dist-weights and observing that the 2nd and 3rd 
  elements are the same. 
  This works, but some tweaking is needed, e.g. using scaling the results.

  Here importance-sampler is used. The p2 and p3 values are exact since they are
  from enumerate.

  var : n
  11: 1.0
  mean: 11.0

  var : p2
  0.258103609: 1.0
  mean: 0.258103609

  var : p3
  0.258103609: 1.0
  mean: 0.258103609

  var : n p3 p3
  (11 0.258103609 0.258103609): 1.0


|#
(define (model2)
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n (+ 3 (random-integer 20)))
   (observe/fail (>= n 3))
   
   (define p 1/4)

   (define weights (discrete-dist-weights (enumerate (binomial n 1/4))))
   (define scale 1000000000)
   (define w2 (roundf (vector-ref weights 2) (/ scale)))
   (define w3 (roundf (vector-ref weights 3) (/ scale)))

   (observe-sample (dist-unit w2) w3)
      
   (list n
         w2
         w3
         (list n w2 w3)
         )
   

   )
)

(show-marginals (model2)
                (list  "n"
                       "p2"
                       "p3"
                       "n p3 p3"
                       )
                #:num-samples 100
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


