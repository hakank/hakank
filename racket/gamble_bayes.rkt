#| 

  Bayes (Resampling Stats) in Racket.Gamble 

  From Statistics101 (Resampling Stats)
  Program bayes.txt
  """
  Suppose 2 percent of the population has a particular disease.
  There is a test for the disease, but it is not perfectly accurate,
  as 3.2 percent of the population tests positive for the disease.
  There is a 75 percent chance that a person with the disease will
  test positive. Calculate the probability that a person who does 
  test positive really does have the disease.
  Solution: Out of 1000 people, 32 (3.2 percent) will test positive;
  but only 20 (2 percent) actually have the disease.

  NAME hasDisease noDisease
  NAME truePositive falsePositive
  COPY 2#hasDisease 98#noDisease population
  COPY 20#truePositive 12#falsePositive testPopulation

  REPEAT 10000
    SAMPLE 1 testPopulation testResult
    IF testResult = truePositive
      SAMPLE 1 population actualHealth
      IF actualHealth = hasDisease
         SCORE hasDisease results
      ELSE
         SCORE noDisease results
      END
    END
  END
  COUNT results = hasDisease truePositivesCount
  COUNT results = noDisease falsePositiveCount
  ADD truePositivesCount falsePositiveCount totalPositives
  DIVIDE truePositivesCount totalPositives probability
  PRINT probability

  -> probability: 0.019688790092092727
  """

  Here is both a Gamble model as well as a resampling approach.


  Using enumerate for the model part:  
  var : testResult
  true positive: 5/8 (0.625)
  false positive: 3/8 (0.375)

  var : actualHealth
  no disease: 49/50 (0.98)
  has disease: 1/50 (0.02)

  Note: When using enumerate the resampling part does not work as intended.

  Using importance-sampler for both parts:

  var : testResult
  true positive: 0.6388999999999999
  false positive: 0.36110000000000003

  var : actualHealth
  no disease: 0.9797
  has disease: 0.0203

  (result.length: 6389 all.length: 10000)
  (tp: 124 pct: 0.019408358115511035)
  (fp: 6265 pct: 0.980591641884489)
  (tests positive (tp+fp): 6389 pct: 0.0124)



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;; 2% of the population has the disease
(define population (append (ones-list 2 "has disease") (ones-list 98 "no disease")))

(define testPopulation (append (ones-list 20 "true positive") (ones-list 12 "false positive")))

(define result '()) ;; The one with true positive: has/n disease
(define all '()) ;; Count all the observations

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define testResult (uniform-draw testPopulation))
   (define actualHealth (uniform-draw population))
   (set! all (append all (list 1)))
   
   (when (eq? testResult "true positive")
     (if (eq? actualHealth "has disease")
         (set! result (append result (list "has disease")))
         (set! result (append result (list "no disease")))))
    
   (list testResult
         actualHealth
         )
   
   )
)

(show-marginals (model)
                (list  "testResult"
                       "actualHealth"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


(define result_len (length result))
(define all_len (length all))
(show2 "result.length:" result_len "all.length:" all_len)
(define tp (count-occurrences-eq "has disease" result))
(define fp (count-occurrences-eq "no disease" result))

(show2 "tp:" tp "pct:" (* 1.0 (/ tp result_len)))
(show2 "fp:" fp "pct:" (* 1.0 (/ fp result_len)))
(show2 "tests positive (tp+fp):" (+ tp fp) "pct:" (* 1.0 (/ tp all_len)))
