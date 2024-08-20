#| 

  Heart disease regression in Racket Gamble.

  From Hugin's BN heart_disease_regression.net
   
  Risk of dying from heart disease in the next 10 years

  Note: Using the -dist distributions, it does not work with (observe-sample)
  Even with mem'ing sex, age, and cholesterol abd
     observe-sample   sex = male
  the value of sex is random 50/50. Why?
  Both importance-sampler and mh-sampler has this behavior...

  However, using (observe/fail) and using intervals it works.

var : sex
mean: 0 (0.0)

var : age
mean: 66.96615279697821

var : cholesterol
mean: 5.690437826282082

var : score_cholesterol
mean: 0.8285253915383812

var : score_sex
mean: 0 (0.0)

var : score_age
mean: 33.93230559395448

var : score
mean: 29.76083098549276

var : risk
mean: 0.9999999999998811

var : 1 - risk
mean: 1.3948959765031377e-13

  Darn, according to this model, there's a probability of 0.9999999983124701
  that I will die from a hear disease in the next 10 years...
  (and it was 

  Lowering the cholesterol to 2.5..2.9 it is still a 99.99...% risk of dying of
  a heart related disease.



  This is a port of my WebPPL model heart_disease_regression.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (heart-disease-regression)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define male 0)
   (define female 1)

   ;
   ; Priors 
   ; Note the mem's
   ;
   
   ; (define (sex i) (random-integer-dist 2))
   (define sex (mem (lambda (i) (random-integer 2))))
      
   ;; In the Hugin model the variance is 100.0.   
   ; (define (cholesterol i) (normal-dist 6 10))
   (define cholesterol (mem (lambda (i) (normal 6 10))))
   
   ;; In the Hugin model the variance is 625.
   ; (define (age i) (normal-dist 40 25))
   (define age (mem (lambda (i) (normal 40 25))))

   ;
   ; The scores
   ;
   
   (define intercept -5)
   
   (define score_cholesterol (* 1.2 (- (cholesterol 0)  5)))  
   (define score_sex (* -1 (sex 0)))
   (define score_age (* 2 (- (age 0) 50)))
   
   (define score (+ intercept score_age score_sex score_cholesterol))

   ; Formula:
   ; Risk of dying from heart disease in the next 10 years
   (define risk (/ 1 (+ 1 (exp (- score)))))

   ;
   ; Observations
   ;
   
   ; sex
   (observe/fail (eq? (sex 0) male))

   ; age
   (observe/fail (and (>= (age 0) 66.5) (<= (age 0) 67.5)))
   ; (observe/fail (= (age 0) 67)) ; This takes - unsurprisingly - forever 
   ; (observe/fail (>= (age 0) 60))

   ; cholesterol
   ;; ;; (observe/fail (>= cholesterol 1))
   (observe/fail (and (>= (cholesterol 0) 5.5) (<= (cholesterol 0) 5.9)))
   ; (observe/fail (and (>= (cholesterol 0) 1.5) (<= (cholesterol 0) 1.9))) 

   ;;;
   ;;; observe-sample does not work for this model for some reason
   ;;; (with the appropritate changes in the model). Why?
   ;;;
   ;; (observe-sample (sex 0) male)
   ;; (observe-sample (age 0) 63)
   ;; (observe-sample (cholesterol 0) 5.5)  

   (list (sex 0)
         (age 0)
         (cholesterol 0)
         score_cholesterol
         score_sex
         score_age
         score
         risk
         (- 1 risk)
         )
         
   )
  )

(show-marginals (heart-disease-regression)
                (list "sex"
                      "age"
                      "cholesterol"
                      "score_cholesterol"
                      "score_sex"
                      "score_age"
                      "score"
                      "risk"
                      "1 - risk"
                      )
                #:num-samples 10000
                #:truncate-output 4
                #:skip-marginals? #t
                )

