#| 

  Murder mystery in Racket.Gamble 

  From https://mbmlbook.com/MurderMystery.html
  
  Infer.Net code: 
  https://github.com/dotnet/mbmlbook/blob/main/src/1.%20A%20Murder%20Mystery/Program.cs
  """
  ;; Input probabilities
  (define priors = new MurdererProbs ( Grey = 0.3, Auburn = 0.7 );
  
  (define conditionalsWeapon = new Variables.ConditionalVariablesWeapon
  (
  RevolverGivenGrey = 0.9,
  DaggerGivenGrey = 0.1,
  RevolverGivenAuburn = 0.2,
  DaggerGivenAuburn = 0.8,
  );
  
  (define conditionalsHair = new Variables.ConditionalVariablesHair
  (
  HairGivenGrey = 0.5,
  HairGivenAuburn = 0.05
  );
  """

  * weapon_cond: #t hair_cond #t
  var : murderer
  grey: 0.9507042253521127
  auburn: 0.04929577464788734

  var : murderer_gray
  #t: 0.9507042253521127
  #f: 0.04929577464788734
  mean: 0.9507042253521127

  var : murderer_auburn
  #f: 0.9507042253521127
  #t: 0.04929577464788734
  mean: 0.04929577464788734

  var : weapon
  revolver: 1.0

  var : hair
  #t: 1.0
  mean: 1.0

  * weapon_cond: #t hair_cond #f
  var : murderer
  grey: 0.6585365853658536
  auburn: 0.3414634146341464

  var : murderer_gray
  #t: 0.6585365853658536
  #f: 0.3414634146341464
  mean: 0.6585365853658536

  var : murderer_auburn
  #f: 0.6585365853658536
  #t: 0.3414634146341464
  mean: 0.3414634146341464

  var : weapon
  revolver: 1.0

  var : hair
  #f: 0.6536585365853659
  #t: 0.3463414634146341
  mean: 0.3463414634146341

  * weapon_cond: #f hair_cond #t
  var : murderer
  grey: 0.8108108108108107
  auburn: 0.1891891891891892
  
  var : murderer_gray
  #t: 0.8108108108108107
  #f: 0.1891891891891892
  mean: 0.8108108108108107

  var : murderer_auburn
  #f: 0.8108108108108107
  #t: 0.1891891891891892
  mean: 0.1891891891891892

  var : weapon
  revolver: 0.7675675675675676
  dagger: 0.23243243243243242

  var : hair
  #t: 1.0
  mean: 1.0

  * weapon_cond: #f hair_cond #f
  var : murderer
  auburn: 0.7000000000000001
  grey: 0.30000000000000004
  
  var : murderer_gray
  #f: 0.7000000000000001
  #t: 0.30000000000000004
  mean: 0.30000000000000004

  var : murderer_auburn
  #t: 0.7000000000000001
  #f: 0.30000000000000004
  mean: 0.7000000000000001

  var : weapon
  dagger: 0.5900000000000001
  revolver: 0.41000000000000003

  var : hair
  #f: 0.8150000000000001
  #t: 0.185
  mean: 0.185


  This is a port of my WebPPL model murder_myster_mbmlbook.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (model weapon_cond hair_cond)
  (displayln (format "* weapon_cond: ~a hair_cond ~a" weapon_cond hair_cond))
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ;; Priors for the murderer
   (define murderer (categorical-vw2 (vector 0.3 0.7) (vector "grey""auburn")))

   (define murderer_gray (eq? murderer "grey"))
   (define murderer_auburn (eq? murderer "auburn"))
          
   ;; The weapon was found
   (define weapon
     (if (eq? murderer "grey" )
         (categorical-vw2 (vector 0.9 0.1) (vector "revolver" "dagger"))
         (categorical-vw2 (vector 0.2 0.8) (vector "revolver""dagger"))))
   
   
   ;; Some hair was found
   (define hair (if (eq? murderer "grey") (flip 0.5) (flip 0.05)))

   (when weapon_cond (observe/fail (eq? weapon "revolver")))
   (when hair_cond  (observe/fail hair))

   (list murderer
         murderer_gray
         murderer_auburn
         weapon
         hair
        )

   )
)

(for* ([weapon_cond '(#t #f)]
       [hair_cond '(#t #f)])
  (show-marginals (model weapon_cond hair_cond)
                  (list  "murderer"
                         "murderer_gray"
                         "murderer_auburn"
                         "weapon"
                         "hair"
                         )
                  #:num-samples 1000
                  ; #:truncate-output 1
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )
  )

