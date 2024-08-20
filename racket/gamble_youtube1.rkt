#| 

  Conditional probability Racket Gamble.

  From 
  "<lambda>PSI - Exact Inference for Higher Order Probabilistic Programs"
  https://www.youtube.com/watch?v=GraKkfALsFY
  @0:12

  PSI model
  """
  def main() {
    p := uniform(0,1);
    r := 0;
    for i in [0..10] {
      r += flip(p);
    }

    observe(r % 3 == 0);
  
    return (p);
  
}
  """

  * Model 1: Using set! (corresponding to the logic in the PSI model)
  * Model 2: set! free model.

  Both give the same result (modulo the randomness of stuff). Here's from 
  a run of model 1:

  var : p
  0.5202863649976356: 3.333333333333668e-5
  0.06279035309804451: 3.333333333333668e-5
  0.6082706142962649: 3.333333333333668e-5
  0.18911780052271265: 3.333333333333668e-5
  ...
  0.5452014986430089: 3.333333333333668e-5
  0.1399014680878039: 3.333333333333668e-5
  0.9940339382176892: 3.333333333333668e-5
  0.020976801953086353: 3.333333333333668e-5
  mean: 0.4557993560935153

  var : r
  0: 0.25593333333334417
  9: 0.2495666666666782
  3: 0.2489000000000116
  6: 0.24560000000001198
  mean: 4.46640000000021



  This is a port of my PSI model youtube1.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

; set! model
(define (model)

  (; enumerate ; strange results
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define p (uniform 0 1))
   (define r 0)
   (for ([i 10])
     (set! r (+ r (bernoulli p)))
     )

   (observe/fail (= (modulo r 3) 0))
  
   (list p
         r
         )
  
   )
  )

(displayln "Model 1: using set!")
(show-marginals (model)
                (list "p"
                      "r"
                      )
                #:num-samples 30000
                #:truncate-output 4
                ; #:skip-marginals? #t
                #:credible-interval 0.94
                ; #:show-stats? #t
                )


; set! free model
(define (model2)

  (; enumerate ; cannot enumerate from uniform
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define p (uniform 0 1))
   ; It's set!-free but still using a loop...
   (define r (for/sum ([i 10]) (bernoulli p)))

   (observe/fail (= (modulo r 3) 0))
  
   (list p
         r
         )
  
   )
  )

(displayln "Model 2 (set! free)")
(show-marginals (model2)
                (list "p"
                      "r"
                      )
                #:num-samples 10000
                #:truncate-output 4
                ; #:skip-marginals? #t
                #:credible-interval 0.94
                ; #:show-stats? #t
                )




