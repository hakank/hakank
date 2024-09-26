#| 

  Virus infection in Racket.Gamble 

  From https://www.reddit.com/r/Probability/comments/1fa52ig/help_with_exercise_conditional_probabilities_let/
  """
  Help with exercise conditional probabilities

  Hello .. how would you solve this exercise ?

  A disease can be caused by three viruses A, B, and C. In a laboratory there are three tubes with virus A, 
  two with virus B, and five with virus C. The probability that virus A causes the disease is 1/3, 
  virus B is 2/3, and virus C is 1/7. A virus is inoculated into an animal and it contracts the disease. 
  What is the probability that the inoculated virus was C?

  I think I should calculate the P (incoulated C| disease)= (P disease C|inoculated C * P inoculated C) / P disease= 6.25%

  Can you confirm that? i have no solution for this exercise

  Thank you for your help
  """

  * Model 1:

    var : virus
    B: 7/16 (0.4375)
    A: 21/64 (0.328125)
    C: 15/64 (0.234375)

  The probability of C caused the virus is about 23.4%,

  See below (model2) for an alternative approach.


  A shorter version w/o any fancy Probabilitic Programming stuff:
  > (/ (* 5. 1/7) (+ (* 3 1/3) (* 2 2/3) (* 5 1/7)))
  0.23437499999999997

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate ; #:limit 1e-02
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define viruses (vector "A" "B" "C"))
  
   ; 3 virus A tubes, 2 virus B tubes, and 5 with virus C   
   (define num-tubes '(3 2 5))
   (define probs '(1/3 2/3 1/7))
   
   ; The probability that virus A causes the disease is 1/3, virus B is 2/3, and virus C is 1/7
   ; (define virus (categorical-vw2 (vector (* 3 1/3) (* 2 2/3) (* 5 1/7)) (vector "A" "B" "C")))
   
   ; This is more "formal":
   (define combined (list->vector (map (lambda (t p) (* t p)) num-tubes probs)))
   (define virus (categorical-vw2 combined viruses ))

   (list virus
         )
   
   )
)

(displayln "Model 1")
(show-marginals (model)
                (list  "virus"
                     )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

#|
  Another approach
  (as it happens it's quite similar to the Bayesian Network in
   https://www.reddit.com/r/Probability/comments/1fa52ig/help_with_exercise_conditional_probabilities_let/ )

  * We have observed a virus

    var : tube
    B: 7/16 (0.4375)
    A: 21/64 (0.328125)
    C: 15/64 (0.234375)

    var : virus-prob
    #t: 1 (1.0)
    mean: 1 (1.0)


  * Without any observation about the virus

    var : tube
    C: 1/2 (0.5)
    A: 3/10 (0.3)
    B: 1/5 (0.2)

    var : virus-prob
    #f: 73/105 (0.6952380952380952)
    #t: 32/105 (0.3047619047619048)
    mean: 32/105 (0.3047619047619048)


|#
(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define viruses (vector "A" "B" "C"))
   
   ; 3 virus A tubes, 2 virus B tubes, and 5 with virus C                
   (define num-tubes (vector 3 2 5)) 
   (define tube (categorical-vw2 num-tubes viruses))
   
   (define probs (list 1/3 2/3 1/7))
   
   ; The probability that virus A causes the disease is 1/3, virus B is 2/3, and virus C is 1/7
   (define virus
     (case tube
       [("A") (flip 1/3)]
       [("B") (flip 2/3)]
       [("C") (flip 1/7)]))
   
   ;; I thought that this should be more elegant, but it turned out to be messier.
   ;; (define virus (flip (list-ref probs (index-of (vector->list viruses) tube))))
   
   
   ; We know that it's an virus
   (observe/fail virus)
   
   (list tube
         virus
         )

   
   
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "tube"
                       "virus-prob"
                     )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

