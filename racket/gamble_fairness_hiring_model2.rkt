#| 

  Fairness hiring model II in Racket Gamble 

  Port of SPPL model
  https://github.com/probcomp/sppl/blob/master/examples/fairness-hiring-model-2.ipynb

  The SPPL model gives the following exact probabilities:
  p(hire): 0.911360805292618
  p(hire|male):0.9777674365554804
  p(hire|female):0.8449541740297555
  p(hire|female)/p(hire|male): 0.8641668176293485


var : is_male
#f: 0.5073000000000005
#t: 0.4927000000000021
mean: 0.4927000000000021

var : hire
1: 0.9063999999999566
0: 0.093600000000007
mean: 0.9063999999999566

var : college_rank
29.938304455256482: 0.00010000000000000938
11.422279237583965: 0.00010000000000000938
20.508451307450173: 0.00010000000000000938
12.792855686155058: 0.00010000000000000938
23.461298344798145: 0.00010000000000000938
...
13.143163968019527: 0.00010000000000000938
20.879522519937893: 0.00010000000000000938
22.41378014085026: 0.00010000000000000938
17.44584542237019: 0.00010000000000000938
17.23132474967681: 0.00010000000000000938
mean: 24.960668612551096

var : years_exp
23.21238199291551: 0.00010000000000000938
14.690694169084582: 0.00010000000000000938
13.143613427149598: 0.00010000000000000938
-2.6545125440723893: 0.00010000000000000938
14.74884174031659: 0.00010000000000000938
...
15.0193256568968: 0.00010000000000000938
9.61526688849408: 0.00010000000000000938
9.187588737636148: 0.00010000000000000938
11.36837690636525: 0.00010000000000000938
20.77959320416285: 0.00010000000000000938
mean: 12.394311340856914

var : (hire is_male)
(1 #t): 0.4817000000000033
(1 #f): 0.42470000000000957
(0 #f): 0.08260000000000668
(0 #t): 0.011000000000001061

var : hire_given_male
0: 0.5073000000000005
1: 0.4927000000000021
mean: 0.4927000000000021

var : hire_given_female
1: 0.5073000000000005
0: 0.4927000000000021
mean: 0.5073000000000005

(male 531 female 469 female/male 469/531 ( 0.8832391713747646)

  This is a port of my WebPPL model fairness_hiring_model2.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define (model)
  (; enumerate 
   ; rejection-sampler
   ; importance-sampler
   mh-sampler ; #:transition (slice)

   ;; Population model
   (define is_male (flip 0.5))
   (define college_rank (normal 25 10))
   (define years_exp (if is_male (normal 15 5) (normal 10 5)))

   ;; Hiring decision.
   (define hire (if (or (<= college_rank 5) (> years_exp 5)) 1 0))

   (define hire_given_male (if (and hire is_male) 1 0))
   (define hire_given_female (if (and hire (not is_male)) 1 0))
   
   (list is_male
         hire
         college_rank
         years_exp
         (list hire is_male)
         hire_given_male
         hire_given_female
         )
   
   )
  )

(show-marginals (model)
                (list  "is_male"
                       "hire"
                       "college_rank"
                       "years_exp"
                       "(hire is_male)"
                       "hire_given_male"
                       "hire_given_female"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


(let ((male 0)
      (female 0))
  (for ((i 1000))
    (let ((s (sample (sampler->discrete-dist (model) 100))))
      ; (show "s" s)
      (set! male (+ male (list-ref s 5)))
      (set! female (+ female (list-ref s 6)))
      )
    )
  (show2 "male" male "female" female "female/male" (/ female male) "(" (exact->inexact (/ female male)))
  )

