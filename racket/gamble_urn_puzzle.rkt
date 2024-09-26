#| 

  Urn puzzle in Racket.Gamble 

  From Berlin Bayesians
  https://app.slack.com/client/TFPMSKW3F/CFQHMRD6K/thread/CFQHMRD6K-1623812230.000500
  """
  Three urns, first blindly take one from the first and put in the second, 
  then blindly take one from the second and put in the third and then blindly pick 
  one from the third. How likely is it to pick a black one in the last step?

  (
    Urns
    1: white black black
    2: white white black
    3: white black
  )
  """

  var : urn2b
  (black white white black): 2/3 (0.6666666666666666)
  (white white white black): 1/3 (0.3333333333333333)

  var : urn3b
  (white white black): 7/12 (0.5833333333333334)
  (black white black): 5/12 (0.4166666666666667)

  var : ball
  white: 19/36 (0.5277777777777778)
  black: 17/36 (0.4722222222222222)

  var : ball = black
  #f: 19/36 (0.5277777777777778)
  #t: 17/36 (0.4722222222222222)
  mean: 17/36 (0.4722222222222222)


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

   (define urn1 '(white black black))
   (define urn2 '(white white black))
   (define urn3 '(white black))
  
   ;; From urn1 to urn2
   (define urn2b (cons (uniform-draw urn1) urn2))
   
   ;; From urn2b to urn3    
   (define urn3b (cons (uniform-draw urn2b) urn3))

   ;; Pick a random ball from urn3b
   (define ball (uniform-draw urn3b))
   
   (list urn2b
         urn3b
         ball
         (eq? ball 'black)
    )
   )
)

(show-marginals (model)
                (list  "urn2b"
                       "urn3b"
                       "ball"
                       "ball = black"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


