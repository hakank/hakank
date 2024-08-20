#| 

  Intelligence test in Racket Gamble.

  (This is originally a WebPPL example (example/linearRegression.wppl) but translated via my 
  BLOG model linear_regression2.blog.)

  Note: I couldn't get (observe-sample) to work, so it's (observe/fail) allowing
        a small interval (obs-crit).

  The expected values are:
  - m: 2
  - b: 0
  - sigma: ?
  - post 4: 8
  - post 5: 10

  var : m
  mean: 1.9978301341139417
  Credible interval (0.84): 1.9978301341139404..1.9978301341139404

  var : b
  mean: 0.008773822087429311
  Credible interval (0.84): 0.00049910318255658..0.008773822087429306

  var : sigma
  mean: 0.46697036675635056
  Credible interval (0.84): 0.2128956734426425..2.4723704846762242

  var : post 4
  mean: 7.969158510489147
  Credible interval (0.84): 6.023187060475794..9.92771952984207

  var : post 5
  mean: 9.922823863243693
  Credible interval (0.84): 9.136054799750822..12.414942368756634


  This is a port of my WebPPL model linear_regression2.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler
   
   (define xs '(0 1 2 3 4))
   (define ys '[0 2 4 6 8])
   
   (define m (normal 0 1))
   (define b (normal 0 1))
   (define sigma (gamma 1 1))
   
   (define obs-crit 0.01)
   (for* ([i (range (length xs))])
     (let ([y (+ b (* m (list-ref xs i)))])
       ; I haven't got (observe-sample) to work with this.
       ; This is a poor man's observe-sample...
       (observe/fail (<= (abs (- y (list-ref ys i))) obs-crit))
       )
      )

   ; predictions
   (define (post-y i)
     (let ([mu (+ b (* m i))])
       (normal mu sigma))
     )
   
   (list m
         b
         sigma
         (post-y 4)
         (post-y 5)
         )
   
   )
  )

(show-marginals (model)
                (list "m"
                      "b"
                      "sigma"
                      "post 4"
                      "post 5"
                      )
                #:num-samples 1000
                #:truncate-output 4
                #:skip-marginals? #t
                #:credible-interval 0.94
                )


