#| 

  Cookie problem in Racket Gamble.

  From Think Bayes, page 3
  """
  Suppose there are two bowls of cookies. 
  Bowl 1 contains 30 vanilla cookies and 10 chocolate cookies. 
  Bowl 2 contains 20 of each.

  Now suppose you choose one of the bowls at random and, without looking,
  select a cookie at random. The cookie is vanilla. 
  What is the probability that it came from Bowl 1?
  """

  var : cookie
  vanilla: 1 (1.0)

  var : bowl
  bowl1: 3/5 (0.6)
  bowl2: 2/5 (0.4)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (cookie-problem)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define cookies (vector "vanilla" "chocolate"))
   (define bowls   (vector "bowl1" "bowl2"))
    
   (define bowl (uniform-draw bowls))
   (define cookie
     (if (eq? bowl "bowl1")
         (categorical-vw2 (vector 30 10) cookies)
         (categorical-vw2 (vector 20 20) cookies)))
    
   (observe/fail (eq? cookie "vanilla"))

   (list cookie bowl)

   )
  )


(show-marginals (cookie-problem)
                (list "cookie" "bowl")
                )
