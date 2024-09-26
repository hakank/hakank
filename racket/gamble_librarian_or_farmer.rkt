#| 

  Librarian or Farmer problem in Racket.Gamble 

  From Probabilistic Programming and Bayesian Methods for Hackers, page 7ff
  """
  Consider the following story, inspired by Thinking, Fast and Slow by Daniel Kahneman
  (2011).  Steve is described as a shy individual, very helpful, but he has little interest in
  other people. He likes things in their proper order, and is very detailed about his work. Do
  you think Steve is more likely to be a librarian or a farmer? It may seem that Steve is more
  likely to be a librarian, and most people would agree with this conclusion, but that’s
  ignoring the background distribution of librarians and farmers: The ratio of male farmers
  to male librarians is 20:1. Steve is statistically more likely to be a farmer!
  """

  (In the online version of the book, this example seems to have been replaced 
  with a problem of finding bugs in a program.)

  var : job
  farmer: 0.9523809523809529
  librarian: 0.04761904761904763

  var : job_prior
  farmer: 0.9523809523809529
  librarian: 0.04761904761904762

  var : librarian_desc
  #f: 0.5000000000000003
  #t: 0.5000000000000003
  mean: 0.5000000000000003

  var : steve_librarian
  #f: 0.9285714285714292
  #t: 0.07142857142857148
  mean: 0.07142857142857148

  var : steve_farmer
  #t: 0.9285714285714292
  #f: 0.07142857142857148
  mean: 0.9285714285714292


  This is a port of my WebPPL model librarian_or_farmer.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ;; Being a farmer is 20 times more common than being a librarian.
   (define ps (simplex-vector '(1 20)))  ; (vector 1/21 20/21))
   (define vs (vector "librarian" "farmer"))
   (define job_prior (categorical-vw2 ps vs))
   (define job (categorical-vw2 ps vs))

   (define librarian_desc (flip 0.5))

   ; Is Steve a librarian?
   (define steve_librarian (if (and (eq? job "librarian") librarian_desc) (flip 0.95) (flip 0.05)))
   ; Is Steve a farmer? The assumption is that there are only
   ; librarians and farmers in this world.
   (define steve_farmer(not steve_librarian))
    
   (list job
         job_prior
         librarian_desc
         steve_librarian
         steve_farmer
    )

   )
)

(show-marginals (model)
              (list  "job"
                     "job_prior"
                     "librarian_desc"
                     "steve_librarian"
                     "steve_farmer"
                     )
              #:num-samples 1000
              ; #:truncate-output 5
              ; #:skip-marginals? #t
              ; #:show-stats? #t
              ; #:credible-interval 0.84
              ; #:show-histogram? #t
              ; #:show-percentiles? #t
              )


