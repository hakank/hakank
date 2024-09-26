#| 

  Clinical trial in Racket.Gamble 

  From Infer.Net model ClinicalTrial
  https://dotnet.github.io/infer/userguide/Clinical%20trial%20tutorial.html 
  """
  Probability treatment has an effect = Bernoulli(0.7549)  
  Probability of good outcome if given treatment = 0.7142857  
  Probability of good outcome if control = 0.2857143
  """

  var : isEffective
  mean: 0.7558475442852831

  var : probControl
  mean: 0.3429718942334442

  var : probTreatment
  mean: 0.6654246419589808

  var : probAll
  mean: 0.5202134472621592


  This is a port of my WebPPL model clinical_trial.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define control   '( #f #f #t #f #f ))
(define treatment '( #t #f #t #t #t ))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
  
   (define n (length treatment))
  
   (define isEffective   (flip 0.5))
   (define probControl   (beta 1 1))
   (define probTreatment (beta 1 1))
   (define probAll       (beta 1 1))
   
   (for ([i n]) 
     (if isEffective
         (begin 
           (observe-sample (flip-dist probControl)   (list-ref control i))
           (observe-sample (flip-dist probTreatment) (list-ref treatment i))
           )
         (begin
           (observe-sample (flip-dist probAll) (list-ref control i) )
           (observe-sample (flip-dist probAll)  (list-ref treatment i) )
           )
         )
     )

   ; (observe/fail isEffective)
   ; (observe/fail (and isEffective probTreatment))
   ; (observe/fail (and isEffective probControl))
 
   
   (list
    isEffective
    probControl
    probTreatment
    probAll
    )
  )
)

(show-marginals (model)
                (list  "isEffective"
                       "probControl"
                       "probTreatment"
                       "probAll"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


