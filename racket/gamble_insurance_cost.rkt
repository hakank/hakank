#| 

  Insurance cost in Racket/Gamble 

  From Statistics101 (Resampling Stats)
  File insuranceCosts.txt
  """
  From Julian Simon's book "Resampling: The New Statistics" page 410-411:
  A mutual insurance company charges its members according to the risk of
  having an auto accident. It is known that there are two classes of
  people--80 percent of the population with good driving judgment and with a
  probability of .06 of having an accident each year, and 20 percent with poor
  judgment and a probability of .6 of having an accident each year. The
  company's policy is to charge (in addition to a fee to cover overhead
  expenses) $100 for each percent of risk, i.e., a driver with a probability
  of .6 should pay 60*$100 = $6000. If nothing is known of a driver except
  that he had an accident last year, what fee should he pay?
  -> 
  probability: 0.43101415094339623
  insurancePremium: 4310.141509433962
  """

   Here are two models.
  * The first model is a translation of the Statistics101/Resampling Stats program,
    using "external" arrays to calculate (via simulation) probability and premium.

    Two approaches for calculating the insurance cost:

    Calculate insurance premium, first approach:
    hadSecondAccidentA: 779
    noSecondAccidentA: 967
    hadSecondAccidentA:: 779
    noSecondAccidentA:: 967
    probability:: 0.4461626575028637
    insurance premium:: 4461.6265750286375

    Calculate insurance premium, second approach (new samples)
    hadSecondAccidentA: 0.07200000000000001
    noSecondAccidentA: 0.0945
    probability:: 0.43243243243243246
    insurance premium:: 4324.324324324324

    The second approach can be used with enumerate:

    hadSecondAccidentA: 234/3125
    noSecondAccidentA: 291/3125
    probability:: 78/175
    insurance premium:: 4457.142857142857

    But for the first approach enumerate give strange result 
    (because of the reassigments)

 * The second - exact - model is a neater variant without relying to external updates:
   Model 2 (enumerate)
   var : p
   #f: 97/175 (0.5542857142857143)
   #t: 78/175 (0.44571428571428573)
   mean: 78/175 (0.44571428571428573)

   p = true: 78/175
   insurance cost: 4457.142857142857

  This is a port of my WebPPL model insurance_cost.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define goodDriverUniverse (append (rep 6 "accident") (rep 94 "no accident")))
(define badDriverUniverse (append (rep 6 "accident") (rep 4 "no accident")))
(define driverJugdementUniverse (append (rep 2 "bad driver") (rep 8 "good driver")))
(define hadSecondAccidentA 0)
(define noSecondAccidentA 0)

(define (model1)
  (enumerate ; does not give the correct result for approach 1 because of the reassignments but works with approach 2
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define driverType (uniform-draw driverJugdementUniverse))
   (define firstAccidentRecord
     (if (eq? driverType "bad driver")
         (uniform-draw badDriverUniverse)
         (uniform-draw goodDriverUniverse)))
   
   (define secondAccidentRecord
     (if (eq? firstAccidentRecord "accident")
         (if (eq? driverType "bad driver")
             (uniform-draw badDriverUniverse)
             (uniform-draw goodDriverUniverse))
         "no first accident"
         ))
   
   (define hadFirstAccident (eq? firstAccidentRecord "accident"))

   (define hadSecondAccident
     (if (eq? firstAccidentRecord "accident")
         (if (eq? secondAccidentRecord "accident")
             (and (set! hadSecondAccidentA (add1 hadSecondAccidentA)) #t)
             (and (set! noSecondAccidentA (add1 noSecondAccidentA)) #f)
             )
         "no first accident"))
   
   (define hasFirstAndSecondAccident
     (and (eq? firstAccidentRecord "accident") (eq? secondAccidentRecord "accident")))

   ; (observe/fail (eq? firstAccidentRecord "accident"))
   ; (observe/fail hadSecondAccident)
    
   (list driverType
         firstAccidentRecord
         secondAccidentRecord
         hadFirstAccident
         hadSecondAccident
         hasFirstAndSecondAccident
         )
    
   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list "driverType"
                      "firstAccidentRecord"
                      "secondAccidentRecord"
                      "hadFirstAccident"
                      "hadSecondAccident"
                      "hasFirstAndSecondAccident"
                      )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                    ; #:thin 0
                )



(displayln "Calculate insurance premium, first approach:")
;; Using Infer(model) does not yield a correct solutions with this approach. Why?
(show "hadSecondAccidentA" hadSecondAccidentA)
(show "noSecondAccidentA" noSecondAccidentA)
(define yesCount hadSecondAccidentA)
(define noCount noSecondAccidentA)
(show "hadSecondAccidentA:" yesCount)
(show "noSecondAccidentA:" noCount)
(define totalCount (+ yesCount noCount))
(show "probability:" (* 1.0 (/ yesCount totalCount)))
(show "insurance premium:" (* 100.0 100 (/ yesCount totalCount)))

;; Using this approach works with enumerate
(displayln "\nCalculate insurance premium, second approach (new samples)")
(define numSims 10000)

; '(("no first accident" 0.83043) (#f 0.09469) (#t 0.07488))
(define probs
  (get-probs (model1) #:num-samples numSims #:ix 4) )
; probs
(define yesCount2 (get-prob-value probs #t))
(define noCount2 (get-prob-value probs #f))

(show "hadSecondAccidentA" yesCount2)
(show "noSecondAccidentA" noCount2)
(define totalCount2 (+ yesCount2 noCount2))
(show "probability:" (/ yesCount2 totalCount2))
(show "insurance premium:" (* 100.0 100 (/ yesCount2 totalCount2)))



#|
  A neater and exact model.
  However, we have to do external calculations for getting the cost.

|#
(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define accidents_vs (vector "accident" "no accident"))
   (define driverType (categorical-vw2 (vector 2/10 8/10) (vector "bad driver" "good driver")))
   
   (define (accident driver) 
     (if (eq? driver "bad driver")
         (categorical-vw2 (vector 6/10 4/10) accidents_vs)
         (categorical-vw2 (vector 6/100 94/100) accidents_vs)))

   (define firstAccidentRecord (accident driverType))
   (define secondAccidentRecord (accident driverType))
    
   ;; What is the probability of a second accident (given that there's a first accident)
   (define p (eq? secondAccidentRecord "accident"))
    
   ; We are only interested in the cases when there has been a first accident
   (observe/fail (eq? firstAccidentRecord "accident"))

   (list p)
)
)

(displayln "\nModel 2 (enumerate)")
(show-marginals (model2)
                (list "p"))

; Calculating the insurance costs for first and second accidents
(define probs2 (get-probs (model2) #:ix 0) )
probs2
(show "p = true" (get-prob-value probs2 #t))
(show "insurance cost" (* 1.0 100 100 (get-prob-value probs2 #t)))

