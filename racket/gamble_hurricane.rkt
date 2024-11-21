#| 

  Hurricane in Racket/Gamble 

  From BLOG example/hurricane.blog
  """
  Hurricane 
  Figure 4.2 in Milch's thesis

  Number of samples: 1000000
  Distribution of values for First
	B	0.5000740737774813
	A	0.4999259262225148
  Distribution of values for Damage(A)
	Severe	0.6304659440802821
	Mild	0.36953405591971533
  Distribution of values for Damage(B)
	Severe	0.6298433239506385
	Mild	0.3701566760493723
  Distribution of values for Damage(NotFirst)
	Mild	0.7396907319690265
	Severe	0.2603092680309184
  """

  var : First
  A: 1/2 (0.5)
  B: 1/2 (0.5)

  var : Damage A
  Severe: 63/100 (0.63)
  Mild: 37/100 (0.37)

  var : Damage B
  Severe: 63/100 (0.63)
  Mild: 37/100 (0.37)

  var : Damage NotFirst
  Mild: 37/50 (0.74)
  Severe: 13/50 (0.26)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")
(require racket/set)

(define (model)
  (enumerate

   (define cities '("A" "B"))
   (define damageLevel (vector "Severe" "Mild"))
   (define prepLevel (vector "High" "Low"))
   
   (define First (uniform-draw cities))
   (define NotFirst (uniform-draw (remove First cities)))
   
   (defmem (Prep c) 
     (if (eq? First c)
         (categorical-vw2 (vector 1/2 1/2) prepLevel)
         (let ([damageFirst (Damage First)])
           (cond
             [(eq? damageFirst "Severe") (categorical-vw2 (vector 9/10 1/10) prepLevel)]
             [(eq? damageFirst "Mild")   (categorical-vw2 (vector 1/10 9/10) prepLevel)]))))
   
   (defmem (Damage c)
     (let ([PrepC (Prep c)])
       (cond
         [(eq? PrepC "High") (categorical-vw2 (vector 2/10 8/10) damageLevel)]
         [(eq? PrepC "Low")  (categorical-vw2 (vector 8/10 2/10) damageLevel)])))
   
   (observe/fail (eq? (Damage First) "Severe"))
   
   (list First
         (Damage "A")
         (Damage "B")
         (Damage NotFirst)
         )
   
   )
  )

(show-marginals (model)
                (list  "First"
                       "Damage A"
                       "Damage B"
                       "Damage NotFirst"
                     ))


