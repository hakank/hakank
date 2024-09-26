#| 

  Drug trial evaluation in Racket.Gamble 

  Port of PyMC3 Drug trial evaluation model in
  https://docs.pymc.io/pymc-examples/examples/case_studies/BEST.html
  """
  Example: Drug trial evaluation

  To illustrate how this Bayesian estimation approach works in practice, we will use a 
  fictitious example from Kruschke (2012) concerning the evaluation of a clinical trial for 
  drug evaluation. The trial aims to evaluate the efficacy of a "smart drug" that is 
  supposed to increase intelligence by comparing IQ scores of individuals in a treatment 
  arm (those receiving the drug) to those in a control arm (those recieving a placebo). 
  There are 47 individuals and 42 individuals in the treatment and control arms, respectively.
  """

  (Ref: Kruschke JK. Bayesian estimation supersedes the t test. 
     J Exp Psychol Gen. 2013;142(2):573-603. doi:10.1037/a0029146.))

  Point estimates for the PyMC3 model (via the graphs):
  - group1_mean: 102
  - group1_std: 2.1
  - group2_mean: 101
  - group2_std: 1,2
  ~ v: 0.97
  ~ difference_of_means: 1  (1.3% < 0 < 98.7%)


  This model:

var : group1_mean
mean: 101.45532388020783
Credible interval (0.94): 100.06251333841843..103.4066675092008

var : group2_mean
mean: 100.28793728985889
Credible interval (0.94): 99.77044632058184..100.8611110684919

var : group1_std
mean: 6.3588750155101454
Credible interval (0.94): 5.319362700317857..6.9746446955777

var : group2_std
mean: 2.5816692207942937
Credible interval (0.94): 2.242171780525644..3.286217803026853

var : v
mean: 1.0164071778460262
Credible interval (0.94): 1.0001067326300208..1.0563649832410502

var : lambda_1
mean: 0.026140596493513694
Credible interval (0.94): 0.020556814782735727..0.03534115938594468

var : lambda_2
mean: 0.15857113348869128
Credible interval (0.94): 0.08918203360316843..0.18994727139994286

var : diff_of_means
mean: 1.1673865903489136
Credible interval (0.94): -0.06433012480280809..3.6357153757485605

var : diff_of_stds
mean: 3.777205794715873
Credible interval (0.94): 2.4297017388460143..4.659213510369047

var : effect_size
mean: 0.35449838009806334
Credible interval (0.94): -0.02001172067311074..1.1276462344986449


  This is a port of my WebPPL model drug_trial_evaluation.wppl


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice)

   (define drug    '(101.0 100 102 104 102 97 105 105 98 101 100 123 105 103 100 95 102 106 
                         109 102 82 102 100 102 102 101 102 102 103 103 97 97 103 101 97 104 
                         96 103 124 101 101 100 101 101 104 100 101))
   (define placebo '(99 101 100 101 102 100 97 101 104 101 102 102 100 105 88 101 100 
                        104 100 100 100 101 102 103 97 101 101 100 101 99 101 100 100 
                        101 100 99 101 100 102 99 100 99))

    (define y (append drug placebo))
    (define mu_m (avg y))
    ; (define mu_s (expt (stddev y) 2))
    (define mu_s (stddev y)) ; Gamble's normal uses stddev (not variance)

    (define group1_mean (normal mu_m mu_s))
    (define group2_mean (normal mu_m mu_s))

    (define sigma_low 1)
    (define sigma_high 10)
    (define group1_std (uniform sigma_low sigma_high))
    (define group2_std (uniform sigma_low sigma_high))

    (define v (+ 1 (exponential (/ 1 29.0))))
    
    (define lambda_1 (expt group1_std -2))
    (define lambda_2 (expt group2_std -2))

   
    ;; Note: The PyMC3 model uses StudentT which is not supported in WebPPL,
    ;; so a Gaussian was used instead. Gamble has support for StudentT (as t-dist) but I'm not
    ;; sure how the parameters works when applying PyMC's StudentT.
    ;; PyMC's StudentT:
    ;;   group1 = pm.StudentT("drug", nu=nu, mu=group1_mean, lam=lambda_1, observed=iq_drug)
    ;;   group2 = pm.StudentT("placebo", nu=nu, mu=group2_mean, lam=lambda_2, observed=iq_placebo)
    ;; Gamble's t-dist: (degrees mode scale))
    (for ([i (length drug)])
      (observe-sample (normal-dist (+ (* v lambda_1) group1_mean) group1_std) (list-ref drug i)))
      ; (observe-sample (t-dist v group1_mean  lambda_1) (list-ref drug i))) ; ? Give strange results
    
    (for ([i (length placebo)])
      (observe-sample (normal-dist (+ (* v lambda_2) group2_mean) group2_std) (list-ref placebo i)))
      ; (observe-sample (t-dist v group2_mean lambda_2) (list-ref placebo i))) ; ?


    (define diff_of_means (- group1_mean group2_mean))
    (define diff_of_stds  (- group1_std group2_std))
    (define effect_size (/ diff_of_means (/ (sqrt (+ (expt group1_std 2) (expt group2_std 2))) 2) ))

    (list group1_mean
          group2_mean
          group1_std
          group2_std
          v
          lambda_1
          lambda_2
          diff_of_means
          diff_of_stds
          effect_size
    )

   )
)

(show-marginals (model)
                (list  "group1_mean"
                       "group2_mean"
                       "group1_std"
                       "group2_std"
                       "v"
                       "lambda_1"
                       "lambda_2"
                       "diff_of_means"
                       "diff_of_stds"
                       "effect_size"
                       
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


