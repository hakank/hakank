#| 

  Medical test in Racket Gamble.

  https://www.math.hmc.edu/funfacts/ffiles/30002.6.shtml
  """
  Suppose that you are worried that you might have a rare disease. You decide to get tested,
  and suppose that the testing methods for this disease are correct 99 percent of the time
  (in other words, if you have the disease, it shows that you do with 99 percent probability,
  and if you don't have the disease, it shows that you do not with 99 percent probability).
  Suppose this disease is actually quite rare, occurring randomly in the general population
  in only one of every 10,000 people.

  If your test results come back positive, what are your chances that you actually have the disease?

  Do you think it is approximately: (a) .99, (b) .90, (c) .10, or (d) .01?

  Surprisingly, the answer is (d), less than 1 percent chance that you have the disease! 
  """


  var : disease
  #f: 0.9901960784313726
  #t: 0.009803921568627458
  mean: 0.009803921568627458

  var : test
  #t: 1.0
  mean: 1.0


  This is a port of my WebPPL model medical_test.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ;; Suppose this disease is actually quite rare, occurring randomly in the general population
   ;; in only one of every 10,000 people.
   (define prob_of_disease (/ 10000))
   ;; (define prob_of_disease (/ 1000))
    
   ;; Probability that a person has a disease (and there's a reason to do a test)
   (define disease (flip prob_of_disease))
    
   ;; The test is quite accurate: It shows correct result (test is positive if disease) in 99%.
   ;; However, in 1% of the case it shows incorrect result (positive even if there is no disease).
   (define reliability 0.99)    ;; => disease:0.009803921568627446
   ;; (define reliability 0.999) ;; A more reliable test => disease:0.09083469721767588
   ;; (define reliability 0.9999) ;; An even more reliable test => disease:0.500000000000028
    
   (define test (if disease (flip reliability) (flip (- 1 reliability))))
    
   (observe/fail test)
   ;; (observe/fail (not test))
   ;; (observe/fail disease)
   ;; (observe/fail (not disease))

   (list disease
         test
         )
  
   )
  )

(show-marginals (model)
                (list "disease"
                      "test"
                      )
                #:num-samples 10000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.84
                )
