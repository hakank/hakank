#| 

  Length age in Racket/Gamble 

  https://people.kth.se/~dbro/ppl-tutorial-part-1.pdf
  page 34
  """
  Task. Suppose you have the following data: 
     length = (55, 57, 52, 64, 53, 64) and 
     age =    (4, 10, 2, 17, 6, 20), 
  where the tuple (length(i), age(i)) represents the 
  length in cm and age in weeks for a baby. Create a WebPPL script that infers a posterior 
  distribution over the length of six months old babies by using Bayesian linear regression of 
  the form , 
       length(i) = N(alpha + beta*age(i), sigma)
  where N is the normal distribution, length the length, age the age, and alpha beta, and sigma 
  are random variables.  

  Note: there is an example on webppl.org, but you need to write your own solution using 
        a recursive function (not map) and using the observe construct (not factor).

  A1. Visualize the result and compute the expected value using the function expectation(INF), 
      where INF is the call to the Infer function. What is the uncertainty of the estimation? 
      Describe and discuss the results.

  A2. Discuss what reasonable priors for alpha, beta, and sigma can be. Test and explain how 
      different priors affect the result.

  A3. Suppose you extend the data set with one more data point, where the length is 100 cm, 
      and the age is 5 weeks. How do the mean value and the uncertainty in 
      the estimation change?
  """

  * Test 1
  variable : alpha
  mean: 50.58879135167197
  HPD interval (0.93): 49.564506092438776..51.461874787712006

  variable : beta
  mean: 0.704450366534939
  HPD interval (0.93): 0.6035322976612295..0.8084998797085079

  * Test 2: Adding a data point of 5 weeks and 100cm
    It increases the alpha but it makes beta more uncertain:
    Beta is probably positive but perhaps slightly negative.
    It should probably be regarded an outlier...
   
  variable : alpha
  mean: 63.33036511068844
  HPD interval (0.93): 61.77405660250312..64.0305871652351

  variable : beta
  mean: 0.033107650262837164
  HPD interval (0.93): -0.07764818383167085..0.12412240747768917

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define lengths1 '(55 57 52 64 53 64))
(define age1     '( 4 10  2 17  6 20))
; Adding 100cm and 5 weeks 
(define lengths2 '(55 57 52 64 53 64 100))
(define age2     '( 4 10  2 17  6 20 5))

(define (model test)
  (show "test" test)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define lengths (if (= test 1) lengths1 lengths2))
   (define age (if (= test 1) age1 age2))   
   

   (define N (length age))

   (define alpha (uniform 0 100))
   (define beta (uniform -1 1))
   ;; (define sigma (uniform 0.1 4))
   (define sigma 1)
    
   (for ([i N]) 
     (observe-sample (normal-dist (+ alpha (* beta (list-ref age i))) sigma) (list-ref lengths i)))

   (list alpha
         beta
         ; sigma
    )
        
   )
)

(for ([test '(1 2)])
  (show-marginals (model test )
                  (list  "alpha"
                         "beta"
                         "sigma"
                         )
                  #:num-samples 10000
                  #:truncate-output 5
                  #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  #:hpd-interval (list 0.93)
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ; #:burn 0
                  ; #:thin 0
                  )
  )


