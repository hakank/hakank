#| 

  Two aces in Racket/Gamble 

  From Statistics101 (Resample Stats)
  http://www.statistics101.net/QuickReference.pdf
  Page 20
  """
  What is the probability of drawing an ace at random from a
  deck of cards, and then on your second draw drawing another ace?
  ...
  Thus the answer to the question would be 4/52 x 3/51 = .00452.
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define deck (flatten (rep 4 (range 13))))

#|
  Using draw-without-replacement

  variable : p
  #f: 220/221 (0.995475113122172)
  #t: 1/221 (0.004524886877828055)
  mean: 1/221 (0.004524886877828055)

|#
(define (model1)
  (enumerate
   (define sample (draw-without-replacement 2 deck))
   (define p (equal? sample '(1 1)))
   (list p)

   )
)

(displayln "Model 1")
(show-marginals (model1)
              (list  "p"))

#|
  Using hypergeometric2 

  variable : p1
  1/221: 1 (1.0)
  mean: 1/221 (0.004524886877828055)

  variable : p2
  #f: 220/221 (0.995475113122172)
  #t: 1/221 (0.004524886877828055)
  mean: 1/221 (0.004524886877828055)

|#
(define (model2)
  (enumerate
   (define p1 (hypergeometric2_pdf 2 4 52 2))
   (define p2 (= (hypergeometric2 2 4 52) 2))
   (list p1
         p2)

   )
)

(show-marginals (model2)
              (list  "p1" "p2"))


