#| 

  Dice puzzle in Racket Gamble.

  From Berlin Bayesians:
  (https://app.slack.com/client/TFPMSKW3F/CFQHMRD6K)
  """
  What's likelier: Rolling at least one six in four throws of a single die,
  or rolling at least one double 6 in 24 throws of a pair of dice?
  """

  This is de Méré's classical dice puzzle (which induced the study of
  probability theory). See
  - https://mathworld.wolfram.com/deMeresProblem.html

  4 throws getting at least one 6 (exact)
  (#t : 671/1296 (0.5177469135802469))
  (#f : 625/1296 (0.48225308641975306))

  Cf: 1-(5/6)**4  ~ 0.51774691358024691358

  24 throws getting at least one double 6 (rejection-sampler)
  (#f : 0.5182)
  (#t : 0.4818)

  Cf: 1-(35/36)**24 ~ 0.49140387613090325958

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(displayln "4 throws getting at least one 6 (exact)")
(show-model
 (enumerate
  (define n1 4)
  (define t1 (for/sum ([i (range n1)])
               (if (eq? 6 (uniform-draw (range 1 7)))
                   1
                   0)))
  (>= t1 1)
  )
 )



(displayln "\n24 throws getting at least one double 6 (rejection-sampler)")
(define (twenty-four)
  (rejection-sampler
   ; importance-sampler
   
   (define n2 24)
   (define t2 (for/sum ([i (range n2)])
                (if (and
                     (eq? 6 (uniform-draw (range 1 7)))
                     (eq? 6 (uniform-draw (range 1 7)))
                     )
                    1
                    0)))
   (>= t2 1)
   )
  )

; (show-discrete-dist (sampler->discrete-dist (twenty-four) 10000))
(show-model (twenty-four) #:num-samples 10000 #:no-stats? #t #:no-cred? #t)
