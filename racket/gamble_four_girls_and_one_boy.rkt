#| 

  Four girls and one boy in Racket/Gamble 

  From Statistics101
  """
  What is probability of selecting 4 girls and 1 boy,
  in any order, when selecting five students from any 
  25 girls and 25 boys? 
  (From Page 122ff in "Resampling Stats: The New Statistics").
  """

  Both models shows the same result:
  var : num_girls
  2: 750/2303 (0.32566217976552325)
  3: 750/2303 (0.32566217976552325)
  1: 1375/9212 (0.1492618323925315)
  4: 1375/9212 (0.1492618323925315)
  0: 33/1316 (0.02507598784194529)
  5: 33/1316 (0.02507598784194529)
  mean: 5/2 (2.5)

  var : p
  #f: 7837/9212 (0.8507381676074686)
  #t: 1375/9212 (0.1492618323925315)
  mean: 1375/9212 (0.1492618323925315)

  ChatGPT-o1-preview get the same result:
  https://chatgpt.com/share/66f3ad34-4d20-800b-a760-7f05c5f525fb
  """
  Thought for 30 seconds
  ...
  Answer: An exact probability of 1375 divided by 9212, that is, probability = 1375⁄/9212.
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (enumerate

   (define n 25) ; n boys and n girls
   (define m 5)  ; pick m children
   (define boys (ones-list n "boy"))
   (define girls (ones-list n "girl"))   
   (define children (append boys girls))
   (define selected (draw-without-replacement m children))

   ;; How many girls?
   (define num_girls (count-occurrences-eq "girl" selected))
   ;; 4 girls?
   (define p (= num_girls 4))
    
   (list num_girls
         p
    )

   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list  "num_girls"
                       "p"
                     ))


; A simpler model
(define (model2)
  (enumerate

   ; 50 boys (0) and girls (1)
   (define d (for/list ([i 50]) (bernoulli 1/2)))
   
   ; Pick 5, How many girls?
   (define num_girls (for/sum ([i 5]) (d i)))
   
   ; 4 girls?
   (define p (= num_girls 4))
   (list num_girls
         p
         )
   )
)

(displayln "Model 2")
(show-marginals (model1)
                (list  "num_girls"
                       "p"
                     ))


