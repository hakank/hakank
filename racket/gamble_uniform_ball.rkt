#| 

  Uniform ball in Racket.Gamble 

  BLOG example/uniform-ball.blog
  """
  Model file for balls in an urn, allowing observation errors. 
  This version uses a Poisson prior for the number of balls.
  """

  Result of the BLOG model:
  """
  Distribution of values for size({b for Ball b : true})
  	1	0.07696467881871667
	2	0.2749155437572819
	3	0.3066400745347743
	4	0.34147970288922114
  Distribution of values for (ballDrawn(Draw[0]) = ballDrawn(Draw[1]))
	false	0.7070034886998044
	true	0.29299651130018706
  """
  
  * importance-sampler
  
  var : numBalls
  4: 0.334
  3: 0.31100000000000005
  2: 0.28300000000000003
  1: 0.07200000000000001
  mean: 2.907

  var : ball 0 == ball 1
  #f: 0.7180000000000001
  #t: 0.28200000000000003
  mean: 0.28200000000000003

  * enumerate #:limit 1e-05
  var : numBalls
  4: 0.34056432280542703
  3: 0.3129612049457228
  2: 0.2691404293278949
  1: 0.07733404292095529
  mean: 2.9167558076356217

  var : ball 0 == ball 1
  #f: 0.7000242795286601
  #t: 0.2999757204713399
  mean: 0.2999757204713399


  This is a port of my WebPPL model uniform_ball.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define n 8)

   (define numBalls (add1 (random-integer 4)))

   (defmem (isBlue b) (flip 0.5))   

   (defmem (ballDrawn d) (random-integer numBalls))

   (defmem (obsBlue d)
        (if (isBlue (ballDrawn d))
            (flip 0.8)
            (flip 0.2);
        ))
    
    #| 
       """
       Evidence file asserting that the drawn balls appeared blue on half the 
       draws and green on half the draws.
       """
     |#
    (observe-sample (dist-unit (obsBlue 0)) #t)
    (observe-sample (dist-unit (obsBlue 1)) #f)
    (observe-sample (dist-unit (obsBlue 2)) #t)
    (observe-sample (dist-unit (obsBlue 3)) #f)
    (observe-sample (dist-unit (obsBlue 4)) #t)
    (observe-sample (dist-unit (obsBlue 5)) #f)
    (observe-sample (dist-unit (obsBlue 6)) #t)
    (observe-sample (dist-unit (obsBlue 7)) #f)
  
    (list numBalls
          (eq? (ballDrawn 0) (ballDrawn 1))
          )

   )
)

(show-marginals (model)
                (list  "numBalls"
                       "ball 0 == ball 1"
                     )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


