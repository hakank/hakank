#| 

  Appears tall in Racket.Gamble 

  From HAL-ProbLog example examples/appears_tall.pl  
  (https://bitbucket.org/pedrozudo/hal_problog/src/master/examples/appears_tall.pl)
  """
  2/5::male;3/5::female.

  normal(180,8)~height:-male.
  normal(160,8)~height:-female.
  is_tall:-male, height~=Height, conS(180<Height).
  is_tall:-female, height~=Height, conS(170<Height).
  appears_tall:- is_tall.
  3/10::appears_tall:-male.

  query(is_tall).
  query(appears_tall).
  """

  * Obervation: is_tall = true

  var : gender
  male: 0.7778200000024589
  female: 0.22218000000036095

  var : height
  183.7386341175581: 1.0000000000019164e-5
  188.17620755123488: 1.0000000000019164e-5
  191.62799998960318: 1.0000000000019164e-5
  191.36810046586845: 1.0000000000019164e-5
  186.42273831390744: 1.0000000000019164e-5
  ...
  196.47852139025974: 1.0000000000019164e-5
  184.07561007857794: 1.0000000000019164e-5
  172.8197703094088: 1.0000000000019164e-5
  188.41139015956253: 1.0000000000019164e-5
  187.58657246444795: 1.0000000000019164e-5
  mean: 181.34533401372136
  Credible interval (0.93): 169.9693128099321..195.35175158906358

  var : appears_tall
  mean: 1.0000000000039144

  var : is_tall
  #t: 0.8214100000027444
  #f: 0.17859000000031736
  mean: 0.8214100000027444


  * No observation

  var : gender
  female: 0.6018999999999901
  male: 0.3981000000000125

  var : height
  157.51517091644678: 0.00010000000000000938
  165.52382856570142: 0.00010000000000000938
  162.26197835454138: 0.00010000000000000938
  167.0689520919497: 0.00010000000000000938
  165.6614037953054: 0.00010000000000000938
  ...
  158.9246424190803: 0.00010000000000000938
  165.7735351394811: 0.00010000000000000938
  180.09203855839036: 0.00010000000000000938
  164.45473067108475: 0.00010000000000000938
  166.05521891108904: 0.00010000000000000938
  mean: 168.48498147919705
  Credible interval (0.93): 147.94815218779493..190.18302943203898

  var : appears_tall
  #f: 0.6619999999999835
  #t: 0.3380000000000191
  mean: 0.3380000000000191
  Credible interval (0.93): 0..1

  var : is_tall
  #f: 0.7237999999999767
  #t: 0.2762000000000259
  mean: 0.2762000000000259
  Credible interval (0.93): 0..1


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler 

   (define genderList (vector "male" "female"))
   (define gender (categorical-vw2 (vector 2/5 3/5) genderList))
   (define height (if (eq? gender "male") (normal 180 8) (normal 160.8 8)))

   (define is_tall (if (eq? gender "male") (>= height 180) (>= height 170)))
   (define appears_tall
     (cond
       [is_tall #t]
       [(eq? gender "male") (flip 3/10)]
       [else #f]))

   ; (observe/fail (eq? appears_tall #t))
   (observe-sample (dist-unit appears_tall) #t)

   (list gender
         height
         appears_tall
         is_tall
         )

   )
)

(show-marginals (model)
                (list  "gender"
                       "height"
                       "appears_tall"
                       "is_tall"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.93
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


