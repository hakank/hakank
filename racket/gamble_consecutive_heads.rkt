#| 

  Consecutive Heads in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/116
  """
  What is the expected number of coin tosses required to get N consecutive heads?
  Denote the term by E[N]

  Answer: Recurrence form: E[N] = 2 E[N](N-1) + 2. Closed form: 2^(N+1) + 2.
  """

  num-heads: 1 theoretical: 2
  var : len
  mean: 2.0139

  num-heads: 2 theoretical: 6
  var : len
  mean: 6.035300000000001
 
  num-heads: 3 theoretical: 14
  var : len
  mean: 13.934500000000007

  num-heads: 4 theoretical: 30
  var : len
  mean: 30.558800000000055

  num-heads: 5 theoretical: 62
  var : len
  mean: 62.98690000000022

  num-heads: 6 theoretical: 126
  var : len
  mean: 125.29160000000063

  num-heads: 7 theoretical: 254
  var : len
  mean: 252.93870000000302

  num-heads: 8 theoretical: 510
  var : len
  mean: 521.0186000000118

  num-heads: 9 theoretical: 1022
  var : len
  mean: 1024.7281000000426


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model [num-heads 4])
  (; enumerate ; #:limit 1e-01
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define num-heads 4)
   (define heads (ones-list num-heads 1))

   (define (f a)
     (if (and (>= (length a) num-heads) (equal? (take-last a num-heads) heads))
          a
          (f (append a (list (bernoulli 1/2))))))

   (define a (f '()))
   (define len (length a))

   (list len)

   )
)

(for ([num-heads (range 1 10)])
  (newline)
  (displayln (format "num-heads: ~a theoretical: ~a" num-heads (- (expt 2 (add1 num-heads)) 2) ))
  (show-marginals (model num-heads )
                  (list  "len")
                  #:num-samples 10000
                  #:skip-marginals? #t
                  ))


