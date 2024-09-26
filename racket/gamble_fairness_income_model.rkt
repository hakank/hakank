#| 

  Fairness income model in Racket Gamble 

  Port of SPPL model fairness-income-model-2.ipynb

  The (original) SPPL model gives the following exact probabilities:
  female_prior: 0.33066502427854466
  female_given_no_hire: 0.33412533774074804
  p_female_given_no hire / p_female: 1.046470962495416

  However, the range of age in the SPPL model is really strange:
  age has a mean of 178 years and the values of capital_gain is way too large: mean = 42765784.8959
  (the decision model checks for capital_gain in the range of 4000...9000).
  
  I guess that second parameter to Normal in the SPPL model is the variance, but it should be std. 
  After sqrt'ing all the second parameters of capital_gain and age, the quantiles are for:
  * age: between 19..65
  * capital gain: between about 500..21000
  
  With adjusted sigma, the SPPL model gives this:
    female_prior: 0.3307616495624872
    female_given_no_hire: 0.21962121326454134
    p_female_given_no hire / p_female:  -33.60136716120389


  I filed an issue about this at the SPPL repo: https://github.com/probcomp/sppl/issues/115
  and the answer was that the data is machine generated so it might not be realistic...

var : sex

var : capital_gain
mean: 9838.13820699295

var : age
mean: 40.507504618990374

var : relationship
mean: 1.9998000000001057

var : t
mean: 0 (0.0)

var : sex_prior

var : sex_female_prior
mean: 0.3254000000000205

var : sex_female_given_no_hire
mean: 0.2175000000000234

(sex_female_prior 0.329 female_given_no_hire 0.218 posterior -33.738601823708215)

  This is a port of my WebPPL model fairness_income_model.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (model)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice)

   (define sex (categorical-vw2 (vector 0.3307 0.6693) (vector "female" "male")))
   
   (define capital_gain (if (eq? sex "female")
                            (normal 568.4105 (sqrt 24248365.5428))
                            (normal 1329.3700 (sqrt 69327473.1006))))
                            
   (define age (if (and (eq? sex "female") (< capital_gain 7298.0000))
                   (normal 38.4208 (sqrt 184.9151))
                   (normal 38.6361 (sqrt 187.2435))))
   
   (observe/fail (> age 18))
   
   (define relationship (if (and (eq? sex "female") (< capital_gain 7298.0000))
                            (categorical-vw2 (vector 0.0491 0.1556 0.4012 0.2589 0.0294 0.1058) (vector 0 1 2 3 4 5))
                            (categorical-vw2 (vector 0.0416 0.1667 0.4583 0.2292 0.0166 0.0876) (vector 0 1 2 3 4 5))))
    
    
   ;; Decision model.
   ;; (What I understand is that t is hire (1) / no hire (0)
   (define t
     (cond
       [(= relationship 1) (if (< capital_gain 5095.5) 1 0)]
       [(= relationship 2) (if (< capital_gain 4718.5) 1 0)]
       [(= relationship 3) (if (< capital_gain 5095.5) 1 0)]
       [(= relationship 4) (if (< capital_gain 8296  ) 1 0)]
       [(= relationship 5) 1]
       [(< capital_gain 4668.5) 1]
       [else 0]))
     
   ;; Prior and posterior
   (define sex_prior (categorical-vw2 (vector 0.3307 0.6693) (vector "female" "male")))
   (define sex_female_prior (eq? sex_prior "female"))
   (define sex_female_given_no_hire (eq? sex "female")) ;; posterior from the model

   ;; Observation: No hire
   (observe/fail (= t 0))

  
   (list sex
         capital_gain
         age
         relationship
         t
         sex_prior
         sex_female_prior
         sex_female_given_no_hire
    )
   
   )
  )

(show-marginals (model)
                (list  "sex"
                       "capital_gain"
                       "age"
                       "relationship"
                       "t"
                       "sex_prior"
                       "sex_female_prior"
                       "sex_female_given_no_hire"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

; Get the ref'th element for each sample in samples
(define (samples-list-ref samples ref)
  (if (boolean? (list-ref (list-ref samples 0) ref))
      (map (lambda (v) (boolean->integer (list-ref v ref))) samples)
      (map (lambda (v) (list-ref v ref)) samples)
      )
  )

(define *samples* (repeat (lambda ()  (sample (sampler->discrete-dist (model) 100))) 1000))
(define sex_female_prior         (exact->inexact (avg (samples-list-ref *samples* 6))))
(define sex_female_given_no_hire (exact->inexact (avg (samples-list-ref *samples* 7))))
(define posterior                (* 100 (- (/ sex_female_given_no_hire sex_female_prior) 1)))
(show2 "sex_female_prior" sex_female_prior "female_given_no_hire" sex_female_given_no_hire "posterior" (exact->inexact posterior) )
(newline)
