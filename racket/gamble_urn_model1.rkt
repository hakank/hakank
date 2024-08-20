#| 

  Urn model in Racket Gamble.

  From https://reference.wolfram.com/language/ref/HypergeometricDistribution.html
  """
  Suppose an urn has 100 elements, of which 40 are special.
  ...
  Compute the probability that there are more than 25 special elements in a 
  draw of 50 elements.
    Answer: 0.0120902

  Compute the expected number of special elements in a draw of 50 elements.
    Answer: 20
  """

var : sumSpecial
40: 1.0
mean: 40.0

var : sumSpecialOf50
20: 0.1614
19: 0.1484
21: 0.1471
22: 0.117
18: 0.1167
17: 0.0807
23: 0.0761
16: 0.0443
24: 0.0411
25: 0.022
15: 0.0204
26: 0.0086
14: 0.0081
13: 0.0032
27: 0.0029
12: 0.001
28: 0.0007
11: 0.0002
29: 0.0001
mean: 19.978299999999997

var : moreThan25SpecialOf50
#f: 0.9876999999999999
#t: 0.012299999999999998
mean: 0.012299999999999998


  This is a port of my WebPPL model urn_model1.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (; enumerate ; too slow
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define N 100)
   (define NumSpecial 40)
    
   (define element (mem (lambda (i) 
                          (categorical-vw2 (vector (- N NumSpecial) NumSpecial) (vector "nonspecial" "special")))))
    
   ;; We have exactly 40 special elements (no random there!)
   (define sumSpecial (for/sum( [i N]) (boolean->integer (eq? (element i) "special"))))
      
   ;; Compute the expected number of special elements in a draw of 50 elements.
   (define sumSpecialOf50 (for/sum ([i 50]) (boolean->integer (eq? (element i) "special"))))

   ;; What's the probability that there are more than 25 special elements in a draw of 50 elements
   (define moreThan25SpecialOf50 (> sumSpecialOf50 25))

   (observe/fail (= sumSpecial 40))
   
   (list sumSpecial ;; should be 40!
         sumSpecialOf50
         moreThan25SpecialOf50
         )
   
   )
  )

(show-marginals (model)
                (list "sumSpecial"
                      "sumSpecialOf50"
                      "moreThan25SpecialOf50"
                      )
                #:num-samples 1000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:show-stats? #t
                )

