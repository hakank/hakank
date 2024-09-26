#| 

  Coincidences II in Racket Gamble.

  This is a port (or at least inspired) by old simulations in R
  written in 2003 (in Swedish):
  http://www.hakank.org/sims/coincidence_simulating.html
  and the Swedish blog post "Sammanträffanden - anteckningar vid läsning 
  av Diaconis och Mosteller 'Methods for Studying Coincidences'" 
  ("Coincidences - note from a reading of Diaconis and Mosteller 'Methods for Studying Coincidences'")
  http://www.hakank.org/webblogg/archives/000216.html

  (translated from Swedish)
  """
  The study of coincidences is related to cognitive illusions (which is the current interest). 
  We have bad intuition regarding coincidences which the birthday problems shows: 
  It takes about 23 person for it to be a 50% probability that two persons in this group 
  shares the birthday. Surprising? A common intuition is that it require many more people .
  ...
  Here I simulate some of the most interesting sections in Diaconis' and Mosteller's paper
  'Methods for Studying Coincidences', section "7.1 General-Purpose Models: Birthday Problems" (857ff).
  """ 

  Note: This model implements the more fancy variation where people has more than  
  one attribute that they can have in common.

  Here we show an example with 100 people with the following attributes:
  - the birthday (1..365)
  - number of theater performances during a year (1..500)
  - attending school (1..1000)

  How many of these people has at least one common attribute with some other person?

  The theoretical values (number of persons required for 50% and 95% chance)
  according to Diaconics & Mosteller "Methods for Studying Coincidences"
  p.50 <- 1.2*sqrt(1/sum(1/num.vals))
  p.95 <- 2.5*sqrt(1/sum(1/num.vals))
 
  R code: p.50
  > 1.2*sqrt(1/sum(1/c(365,500,1000)))
  (1) 15.83929
  
  p.95
  > 2.5*sqrt(1/sum(1/c(365,500,1000)))
  (1) 32.99852


  Output of this model 
  (theoretical p_50: 15.83928833289556 p_95: 32.99851736019909)
  var : p
  #t: 1.0
  mean: 1.0

  var : c
  30: 0.11999999999999997
  28: 0.09999999999999998
  26: 0.08999999999999998
  31: 0.08999999999999998
  25: 0.06999999999999999
  29: 0.06999999999999999
  32: 0.04999999999999999
  27: 0.04999999999999999
  36: 0.039999999999999994
  23: 0.039999999999999994
  24: 0.039999999999999994
  33: 0.029999999999999992
  35: 0.029999999999999992
  38: 0.029999999999999992
  21: 0.029999999999999992
  22: 0.029999999999999992
  34: 0.019999999999999997
  20: 0.019999999999999997
  39: 0.009999999999999998
  40: 0.009999999999999998
  42: 0.009999999999999998
  14: 0.009999999999999998
  18: 0.009999999999999998
  mean: 28.659999999999997


  This is a port of my WebPPL model coincidences2.wppl.
  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

#|
  The theoretical values (number of persons required for 50% and 95% chance)
  according to Diaconics & Mosteller "Methods for Studying Coincidences"
  R code:
    p.50 <- 1.2*sqrt(1/sum(1/num.vals))
    p.95 <- 2.5*sqrt(1/sum(1/num.vals))
|#
(define (theoretical a) 
  (define t (sqrt (/ 1 (for/sum ([v a]) (/ v)))))
  (list (* 1.2 t) (* 2.5 t))
)

#|
Here we show an example with 100 people with the following attributes:
- the birthday (1..365)
- number of theater performances during a year (1..500)
- attending school (1..1000)
|#
; (define num_values_per_attribute (list 365)) ; just birthdays   
(define num_values_per_attribute (list 365 500 1000))
(define num_attributes (length num_values_per_attribute))

(define p_theo (theoretical num_values_per_attribute))
(define p_50 (first p_theo))
(define p_95 (second p_theo))
   
(show2 "theoretical p_50:" p_50 "p_95:" p_95)

(define (model)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice) ; #:transition (enumerative-gibbs)
   
   ; (define num_people 16) ;; 50% chance of at least one coincidence
   ;; (define num_people 23) 
   ; (define num_people 33)  ;; 95% change of at least one coincidence
   (define num_people 100)

   (defmem (x i k) 
     (random-integer (list-ref num_values_per_attribute k))
     )
   
   ;; Check all pairs (i < j) and count the number of coincidence.
   (define c 
     (for/sum ([i num_people]) 
       (for/sum ([j num_people])
         (if (< i j)
             (for/sum ([k num_attributes]) 
               (if (= (x i k) (x j k)) 1 0)
               )
             0
             )
         )
       )
     )
   
   ;; Did we get at least one coincidence?
   (define p (> c 0))
    
   (list p
         c
         ; p_50
         ; p_95
         )
   
   )
  )


(show-marginals (model)
                (list "p"
                      "c"
                      ; "p_50"
                      ; "p_95"
                      )
                #:num-samples 100
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:credible-interval2 0.94
                ; #:show-stats? #t
                ; #:show-histogram? #t ; 10 ; #t
                ; #:show-percentiles? #t ; '(0.01 0.2 0.5 0.7 0.99)  ; #t
                ; #:show-percentiles? '(0.01 0.2 0.5 0.7 0.94 0.99 0.99999)  ; #t                
                )
