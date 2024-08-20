#| 

  Gender height  Racket Gamble.

  Identify a person's sex by height.

  Port of my WebPPL model gender_height.wppl

  Given a certain height, what is the probable gender?

   (height 160.0)
   (female : 0.9847543806257743)
   (male : 0.015245619374225765)

   (height 170.0)
   (female : 0.7762770723045709)
   (male : 0.22372292769542917)

   (height 180.0)
   (male : 0.8460433667383871)
   (female : 0.15395663326161296)

   (height 190.0)
   (male : 0.9902035737309142)
   (female : 0.009796426269085926)

   (height 200.0)
   (male : 0.999521275814495)
   (female : 0.00047872418550502695)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (gender-height h)
  (importance-sampler

   (define gender (uniform-draw '("male" "female")))

   ;; From https://en.wikipedia.org/wiki/List_of_average_human_height_worldwide
   ;; Here are the values for Sweden.
   
   ;; This works. Note it'snormal-dist and (sample height)
   (define height (if (eq? gender "male")
                      (normal-dist 181.5 (sqrt 50))
                      (normal-dist 166.8 (sqrt 50))))
   
   (observe-sample height h)

   ;; This works as well, but it needs a range 
   ;; (define height (if (eq? gender "male")
   ;;                    (normal 181.5 (sqrt 50))
   ;;                    (normal 166.8 (sqrt 50))))
   ;; (observe/fail (<= 159.0 height 161.0)) ; This works
     
   gender)
  )

(for ([h '(160.0 170.0 180.0 190.0 200.0)])
  (displayln (list "height" h))
  ; (show-discrete-dist (sampler->discrete-dist (gender-height h) 10000))
  (show-model (gender-height h) #:num-samples 10000 #:no-stats? #t #:no-cred? #t)
)
