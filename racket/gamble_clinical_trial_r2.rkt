#| 

  Clinical trial (R2) in Racket.Gamble 

  with the datafiles 
  - clinical_trial_control.csv  (size 1000)
  - clinical_trial_treatment.csv (size 1000)

  length :1000 mean(control): 0.513 mean(treatment): 0.51

  Output from the R2 model ClinicalTrial.cs (for isEffective)
  """
  Mean: 0.113
  Variance: 0.100331
  Number of accepted samples = 318
  0:00:14:048.362.900
  """

  For the full dataset (with 1000 entries), Gamble throws nan's:
    dict->discrete-dist: contract violation
    expected: (dict/c any/c (>=/c 0))
    given: '(((#f 0.9796801478105733 0.2209223661316128 0.01813176421208954) . +nan.0) ((#f 0.9617483604863669 0.8136451394279122 0.8886587665651666) . +nan.0) ((#t 0.6062383876184955 0.3648606774648726 0.6924195040140907) . +nan.0) ((#t 0.39802021239384777 0.5562...
  
  Hower it works with smaller data slices.

  * For n=20
    var : isEffective
    mean: 0.3450299511297833

    var : probControl
    mean: 0.49506271467061086

    var : probTreatment
    mean: 0.5135769279096848

    var : probAll
    mean: 0.47933745461779453

    Compare with the SPPL model which gives the exact probability of 0.3658327735005218

  * For n=500

    var : isEffective
    mean: 0.031837217945130884

    var : probControl
    mean: 0.5015665935350437

    var : probTreatment
    mean: 0.5268236418193608

    var : probAll
    mean: 0.5151316944376934


  This is the same model as gamble_clinical_trial.rkt (but flip -> bernoulli) and data from 
  the Turing.jl model clinical_trial.jl.


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define control '(0 0 1 1 1 0 1 1 0 0 0 0 0 0 1 0 1 0 0 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 0 0 1 0 0 1 1 1 0 0 1 0 1 0 1 1 0 1 0 0 1 1 1 0 0 0 1 0 1 1 0 0 1 1 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 0 1 1 0 0 0 1 0 1 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 1 1 1 0 0 1 0 0 0 1 0 1 1 0 1 0 0 1 0 1 1 1 1 0 1 1 0 1 0 0 0 1 0 0 1 0 1 0 0 0 1 1 1 1 1 1 1 0 1 1 1 1 0 0 1 1 0 0 1 0 0 0 0 0 1 1 1 0 1 1 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 0 0 0 0 1 0 0 0 0 1 1 1 0 0 1 0 0 1 0 1 1 0 1 0 1 0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 1 0 0 1 0 1 0 0 0 1 1 0 1 0 1 0 0 0 1 0 0 1 0 0 0 1 1 1 1 1 1 0 1 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 0 1 0 1 1 1 1 1 0 1 0 0 1 0 1 1 1 0 1 0 0 1 0 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1 0 0 0 1 0 1 0 1 0 1 0 1 0 1 1 1 1 0 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 0 0 0 1 1 0 0 0 0 0 1 0 0 1 1 1 1 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 1 1 1 1 0 0 1 0 0 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0 0 1 0 1 0 1 0 0 1 0 1 1 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 1 0 1 1 0 0 0 0 0 0 0 1 0 1 0 1 1 0 1 0 1 1 1 0 1 0 1 1 0 0 0 1 1 0 0 0 1 0 0 1 0 1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 1 1 1 0 1 1 1 1 1 0 1 0 0 1 0 0 0 1 0 1 0 0 1 1 1 0 0 0 1 1 1 0 1 1 0 0 1 0 1 1 1 1 1 0 1 1 0 0 1 1 1 0 1 1 0 0 0 1 0 1 1 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1 0 0 0 0 1 0 1 0 1 0 0 1 0 1 0 0 0 0 0 1 1 0 1 0 0 1 0 0 0 0 0 1 1 1 1 1 0 1 1 0 0 0 1 1 1 0 0 1 0 0 1 0 0 0 1 0 1 1 1 1 0 0 0 0 1 0 0 1 0 0 1 0 1 1 1 0 0 0 1 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 0 1 1 0 1 0 1 1 1 1 1 0 1 0 0 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 0 1 1 1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 0 0 0 1 1 0 0 1 0 1 1 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 1 0 1 0 1 0 0 1 0 1 0 0 0 1 1 1 0 0 1 1 1 0 1 0 1 0 0 1 1 0 0 1 0 0 0 1 0 0 0 0 1 0 0 1 0 1 1 0 1 1 1 1 0 1 0 1 0 1 0 0 1 1 0 1 0 1 1 0 0 0 0 1 1 1 0 0 1 0 1 1 1 0 1 1 0 0 1 0 0 0 0 1 0 1 1 0 0 0 0 0 1 1 1 0 0 0 1 0 1 1 0 1 1 1 1 0 1 0 1 1 0 0 0 1 0 1 1 0 0 1 0 0 1 1 1 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 1 1 1 0 1 1 1 0 1 1 1 0 1 1 0 0 0 1 1 1 0 1 0 0 0 1 0 1 1 0 0 0 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 1 0 1 1 1 0 1 1 1 1 0 1 0 0 1 1 0 0 1 0 1 0 0 1 0 1 1 0 1 0 1))

(define treatment '(0 1 0 0 1 1 1 1 1 1 0 0 1 1 0 1 0 0 1 0 1 0 1 0 0 0 0 1 1 0 0 0 1 1 1 0 1 1 1 1 1 1 1 1 0 1 0 1 0 0 1 0 1 1 1 1 1 1 0 0 1 0 1 0 0 0 1 0 1 0 1 0 0 1 1 0 1 0 0 0 1 0 0 1 1 1 1 0 0 1 1 0 1 1 0 0 1 0 0 0 0 0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 1 1 0 1 1 0 0 1 0 1 1 1 0 0 0 1 0 1 1 0 0 0 1 1 1 1 0 0 0 1 0 0 1 1 1 0 1 1 0 0 1 1 0 0 1 0 1 0 1 0 1 1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 1 1 0 1 0 1 0 1 0 0 1 0 0 1 1 1 1 1 0 1 1 0 1 1 1 0 1 0 0 1 0 0 0 0 1 0 0 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 1 0 1 0 0 0 1 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1 1 1 1 0 1 1 0 0 1 1 1 0 0 0 0 0 1 0 1 1 1 1 1 0 0 1 1 0 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 0 1 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 0 1 1 1 1 1 0 0 1 0 0 0 0 1 1 0 1 1 1 1 0 1 1 1 0 1 0 0 1 1 1 1 0 0 1 0 0 1 1 0 0 0 0 1 1 0 1 1 0 1 0 0 0 0 0 1 1 1 0 0 0 1 0 1 1 1 1 1 0 1 1 0 1 0 0 0 0 0 0 0 1 0 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 0 0 1 1 1 1 0 0 0 0 0 0 1 1 0 0 1 1 1 0 1 0 1 0 0 1 1 1 1 0 0 1 1 0 0 0 1 1 0 1 1 0 1 0 1 1 1 1 0 0 0 0 1 0 1 1 0 0 0 0 0 1 1 0 1 0 0 0 0 0 1 0 0 0 1 1 1 0 1 1 1 1 0 1 1 1 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 1 1 1 1 0 1 0 1 0 0 0 1 0 0 0 0 0 1 0 0 1 0 1 1 0 0 1 0 1 1 1 0 0 1 0 1 1 1 0 1 0 1 1 0 1 1 1 0 0 0 1 0 1 0 0 1 0 0 0 1 0 1 0 1 0 0 1 1 1 1 0 0 1 1 0 0 1 0 1 1 1 0 0 1 0 0 0 0 0 0 0 1 1 0 1 0 0 0 1 1 0 0 0 0 1 0 1 0 1 1 1 0 0 1 0 1 0 0 0 0 1 1 0 1 0 1 1 1 0 0 1 0 0 1 1 1 1 0 1 0 1 0 0 1 0 0 0 1 1 1 1 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 0 1 0 1 0 0 1 1 0 0 0 0 1 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 0 1 0 1 1 0 1 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 0 0 1 1 1 0 0 0 0 0 0 0 1 0 1 0 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 1 0 1 1 1 1 1 0 1 1 0 1 1 1 1 0 1 0 0 0 0 0 1 0 1 1 0 1 1 0 1 1 1 1 1 1 0 0 0 0 0 1 1 0 0 1 1 1 1 0 1 0 1 1 1 1 0 0 1 1 0 0 1 1 0 1 1 1 0 1 0 0 0 0 1 0 0 1 1 1 1 1 0 0 0 1 0 0 0 1 1 1 1 0 0 0 0 0 1 1 0 0 0 1 0 1 1 0 1 0 1 1 0 1 1 1 0 1 0 1 0 1 0 0 0 0 0 1 0 1 0 1 1 0 1 1 0 0 1 0 1 1 0 1 0 1 0 1 1 1 0 0 0 0 0 0 1 0 0 1 0 1 0 0))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
  
   ; (define n (length treatment)) ; 1000 is too much!
   ; (define n 20) 
   (define n 500) ; 500 is ok
   ; (define n 600) ; 600 is too much!
  
   (define isEffective   (flip 0.5))
   (define probControl   (beta 1 1))
   (define probTreatment (beta 1 1))
   (define probAll       (beta 1 1))
   
   (for ([i n]) 
     (if isEffective
         (begin 
           (observe-sample (bernoulli-dist probControl)   (list-ref control i))
           (observe-sample (bernoulli-dist probTreatment) (list-ref treatment i))
           )
         (begin
           (observe-sample (bernoulli-dist probAll) (list-ref control i) )
           (observe-sample (bernoulli-dist probAll)  (list-ref treatment i) )
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


