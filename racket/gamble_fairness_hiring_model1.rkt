#| 

  Fairness hiring model I in Racket Gamble 

  Port of SPPL model
  https://github.com/probcomp/sppl/blob/master/examples/fairness-hiring-model-1.ipynb

  The SPPL model gives the following exact probabilities:
    p_hire_given_minority:0.007131626828051439
    p_hire_given_majority:0.01945024229170891
    min/maj: 0.3666600508668959



var : ethnicity
2: 0.8577999999999619
1: 0.1422000000000108
mean: 1.8577999999999346

var : years_experience
8: 0.20010000000002048
7: 0.19630000000001985
6: 0.1572000000000133
9: 0.1483000000000118
10: 0.093800000000007
...
2: 0.0028000000000002628
13: 0.0027000000000002534
1: 0.000500000000000047
14: 0.00040000000000003753
15: 0.00010000000000000938
mean: 7.4785000000006745

var : college_rank
24.189964377785095: 0.00010000000000000938
21.804253126972608: 0.00010000000000000938
17.02883980500049: 0.00010000000000000938
24.352673217844274: 0.00010000000000000938
25.90175425656649: 0.00010000000000000938
...
23.51972928289232: 0.00010000000000000938
21.42443740024199: 0.00010000000000000938
20.458200285986337: 0.00010000000000000938
24.3815702705827: 0.00010000000000000938
12.639051979311827: 0.00010000000000000938
mean: 20.73080279126891

var : hire
0: 0.9771999999999488
1: 0.022800000000002218
mean: 0.022800000000002218

var : (hire ethnicity)
(0 2): 0.8364999999999643
(0 1): 0.14070000000001054
(1 2): 0.02130000000000207
(1 1): 0.0015000000000001408

var : hire_given_minority
0: 0.8577999999999619
1: 0.1422000000000108
mean: 0.1422000000000108

var : hire_given_majority
1: 0.8577999999999619
0: 0.1422000000000108
mean: 0.8577999999999619

min 152 maj 848 min/maj 19/106 ( 0.1792452830188679)


  The laplace distribution is defined in gamble_distributions.rkt

  This is a port of my WebPPL model fairness_hiring_model1.wppl

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
   importance-sampler
   ; mh-sampler ; #:transition (slice)

   (define minority 1)
   (define majority 2)

   (define ethnicity (categorical-vw2 (vector 0.15 0.85) (vector minority majority)))
   (define years_experience (binomial 15 0.5))
   (define college_rank (if (= ethnicity minority) (laplace 25 5) (laplace 20 5)))
    
   ;; Top 50 colleges and at most 20 years of experience.
   (observe/fail (<= college_rank 50))
   (observe/fail (<= years_experience 20))

   ;; Hiring decision (from the underlying decision tree)
   (define hire (if (<= college_rank 5)
                    1
                    (if (> (- years_experience 5) college_rank)
                        1
                        0)))

   (define hire_given_minority (if (and hire (= ethnicity minority)) 1 0))
   (define hire_given_majority (if (and hire (= ethnicity majority)) 1 0))
   
   (list ethnicity
         years_experience
         college_rank
         hire
         (list hire ethnicity)
         hire_given_minority
         hire_given_majority
    )
   
   )
  )

(show-marginals (model)
                (list  "ethnicity"
                       "years_experience"
                       "college_rank"
                       "hire"
                       "(hire ethnicity)"
                       "hire_given_minority"
                       "hire_given_majority"                       
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


(let ([min 0]
      [maj 0])
  (for ([i 1000])
    (let ([s (sample (sampler->discrete-dist (model) 100))])
      ; (show "s" s)
      (set! min (+ min (list-ref s 5)))
      (set! maj (+ maj (list-ref s 6)))
      )
    )
  (show2 "min" min "maj" maj "min/maj" (/ min maj) "(" (exact->inexact (/ min maj)))
  )

