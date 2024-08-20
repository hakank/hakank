#| 

  Racket Gamble.

  This is a port of my PSI model youtube6.wppl 
  (from a now unavailable YouTube video) 
  """
  def main() {
    x := uniform(0,1);
    y := uniform(0,1);
    // z := y - 2*x; // 𝔼[x] = 1/4
    // cobserve(z,0);   
    z := y/x;  // 𝔼[x] = 1/4
    cobserve(z,2);
  
    return  x;
  }
  """

  The (exact) expected value from PSI is E[x] = 1/3 (0.3333)


  I don't know how to use observe-sample on z. However, using the
  (abs) trick in observe/fail give the (approximately) correct result: 0.333

  var : x
  mean: 0.33082838209469373

  var : z
  mean: 1.9996253950120586

  var : y
  mean: 0.66152383276796

 
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
   (define y (uniform 0 1))
   ; How do I make this as a distribution, to use with (observe-sample z 2)?
   (define z (/ y x))

   ; (observe z 2) ; (with uniform-dist): expression is not observable
   ; (observe-sample z 2) ; Nope, z is not a distribution!
   (observe/fail (< (abs (- z 2)) 0.01))
   
   (list x
         z
         y
         )
  
   )
  )

(show-marginals (model)
                (list "x"
                      "z"
                      "y"
                      )
                #:num-samples 1000
                #:truncate-output 4
                #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:show-stats? #t
                )


