#| 

  Conditional probability Racket Gamble.

  This is a port of my PSI model youtube5.wppl 
  (from a now unavailable YouTube video) 
  """
  def main() {
    x := uniform(0,1);
    d := infer((){
            y := uniform(0,1);
            observe(y <= x);
            return y;
         }) :Distribution[ℝ];
    r := sample(d);
    observe(x <= 1/2);

    return r;
  }
  """

  The (exact) expected value from PSI is E[r] = 1/8 (0.125)

  This model:

  var : r
  0.012379786832024266: 6.666666666667335e-5
  0.2971689279217127: 3.3333333333336676e-5
  0.1420497855977983: 3.3333333333336676e-5
  0.1285063174388637: 3.3333333333336676e-5
  ...
  0.01378319129974204: 3.3333333333336676e-5
  0.14267959787448783: 3.3333333333336676e-5
  0.05897312151883014: 3.3333333333336676e-5
  0.22890721625012836: 3.3333333333336676e-5
  mean: 0.12533605740292802
 
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

   (define x (uniform 0 1))
   
   ; Convert to discrete distribution (and take 1 sample)
   (define d (sampler->discrete-dist (importance-sampler
               (define y (uniform 0 1))
               (observe/fail (<= y x))
               y) 1))

   (define r (sample d))
   
   (observe/fail (<= x 1/2))

    (list r
          ; d
          ; x
          )
  
   )
  )

(show-marginals (model)
                (list "r"
                      ; "d"
                      )
                #:num-samples 30000
                #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:show-stats? #t
                )


