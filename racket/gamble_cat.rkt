#| 

  Cat problem in Racket Gamble.

  https://www.youtube.com/watch?v=e1Ykk_CqKTY&t=458s

  Probabilistic Programming: What It Is and How It Works - Noel Welsh

  We can see either 1, 2, or 3 cats.
  There are 3 different enticements:
  
  - Milkshake
  - Fish
  - Nothing
 
  And there are different probabilities how many cats there are given
  an enticement, see below.

  Now: We see 3 cats, what is the probability that it's a milkshake?

  The video got the following (for 3 cats):
   - milkshake: 0.42
   - fish: 0.04
   - nothing: 0.03

  Normalized to percentage (from the video): 

  0.42/(0.42 + 0.04 + 0.03) milkshake
      0.85714285714285714286
  0.04/(0.42 + 0.04 + 0.03) fish
      0.081632653061224489796
  0.03/(0.42 + 0.04 + 0.03)  nothing
      0.061224489795918367347

  Here are two models.
  Model2 seems to be the correct one (i.e. corresponds to the video),
  i.e. where the enticements are mutual exclusive.
  In model1 we accept that fish and milkshake can coexist.

  Model 1
  var : milkshake
  #t: 0.930379746835443
  #f: 0.069620253164557
  mean: 0.930379746835443

  var : fish
  #f: 0.8715189873417721
  #t: 0.1284810126582279
  mean: 0.1284810126582279

  var : nothing
  #f: 0.9658227848101266
  #t: 0.03417721518987343
  mean: 0.03417721518987343

  var : cat
  3: 1.0
  mean: 3.0


  Model 2
  var : enticement
  milkshake: 0.8571428571428571
  fish: 0.08163265306122452
  nothing: 0.061224489795918345

  var : cat
  3: 0.9999999999999999
  mean: 2.9999999999999996

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (cat1)
  (enumerate
   
   (define OneCat 1)
   (define TwoCats 2)
   (define ThreeCats 3)

   ; probability of an enticement in the garden
   (define milkshake (flip 0.6))
   (define fish      (flip 0.1))
   (define nothing   (flip 0.3))
   
    ; Number of cats per enticement
   (define vs (vector OneCat TwoCats ThreeCats))
     
   (define cat
     (cond
        [(and (not nothing) milkshake)  (categorical-vw2 (vector 0.1 0.2 0.7) vs)]
        [(and (not nothing) fish)       (categorical-vw2 (vector 0.2 0.4 0.4) vs)]
        [(and nothing (not fish) (not milkshake)) (categorical-vw2 (vector 0.6 0.3 0.1) vs)]
        [else #f]))
    
   (observe/fail (eq? cat ThreeCats))

   (list milkshake fish nothing cat)

   )
  )

(displayln "\nModel 1")
(show-marginals (cat1)
                (list "milkshake" "fish" "nothing" "cat")
                )




(define (cat2)
  (enumerate
   
   (define OneCat 1)
   (define TwoCats 2)
   (define ThreeCats 3)

   ; probability of an enticement in the garden
   (define enticement (categorical-vw2 (vector 0.6 0.1 0.3) (vector "milkshake" "fish" "nothing")))
   
    ; Number of cats per enticement
   (define vs (vector OneCat TwoCats ThreeCats))

   (define cat
     (case enticement
        [("milkshake") (categorical-vw2 (vector 0.1 0.2 0.7) vs)]
        [("fish")      (categorical-vw2 (vector 0.2 0.4 0.4) vs)]
        [("nothing")   (categorical-vw2 (vector 0.6 0.3 0.1) vs)]))
    
   (observe/fail (= cat ThreeCats))

   (list enticement cat)

   )
  )

(displayln "\nModel 2")
(show-marginals (cat2)
                (list "enticement" "cat")
                )




