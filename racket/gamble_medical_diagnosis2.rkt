#| 

  Medical diagnosis in Racket.Gamble 

  From https://probmods.org/chapters/conditioning.html
  (WebPPL):
  """
  This classic Bayesian inference task is a special case of conditioning. Kahneman and Tversky, 
  and Gigerenzer and colleagues, have studied how people make simple judgments like the following:

  The probability of breast cancer is 1% for a woman at 40 who participates in a routine screening. 
  If a woman has breast cancer, the probability is 80% that she will have a positive mammography. 
  If a woman does not have breast cancer, the probability is 9.6% that she will also have a 
  positive mammography. A woman in this age group had a positive mammography in a routine 
  screening. What is the probability that she actually has breast cancer?

  What is your intuition? Many people without training in statistical inference judge the 
  probability to be rather high, typically between 0.7 and 0.9. The correct answer is much 
  lower, less than 0.1, as we can see by running this WebPPL inference:
  """

  var : breastCancer
  #f: 0.922360248447205
  #t: 0.07763975155279508
  mean: 0.07763975155279508

  var : positiveMammogram
  #t: 1.0
  mean: 1.0


  This is a port of my Turing.jl model medical_diagnosis2.jl

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

   (define breastCancer (flip 0.01))
   (define positiveMammogram (if breastCancer (flip 0.8) (flip 0.096)))

   (observe/fail positiveMammogram)

   (list breastCancer
         positiveMammogram
         )
   
   )
  )

(show-marginals (model)
                (list  "breastCancer"
                       "positiveMammogram"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


