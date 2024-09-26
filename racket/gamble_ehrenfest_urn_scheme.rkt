#| 

  Ehrenfest urn scheme in Racket/Gamble 

  From Polya Urn Models, page 21
  """
  An Ehrenfest urn starts out with one white and one blue ball. Balls are sampled from 
  the urn. Whenever a ball of a certain color is picked, we discard that ball and replace 
  in the urn a ball of the opposite color. The urn always returns to the initial state 
  after an even number of draws, and is always out of that state after an odd number 
  of draws. When it is out of the initial state, it can be in one of two equally 
  probable states: A state with two white balls or a state with two blue balls.
  """

  Here we randomize the number of even and odd draws: 
  (0..2..8) and (1..2..9) draws, respectively.


  * white init: 1 black_init: 2

  var : num_white_even
  1: 26449/32805 (0.8062490474013108)
  3: 6356/32805 (0.1937509525986892)
  0: 0 (0.0)
  mean: 45517/32805 (1.3875019051973785)

  var : num_blue_even
  2: 26449/32805 (0.8062490474013108)
  0: 6356/32805 (0.1937509525986892)
  4: 0 (0.0)
  mean: 52898/32805 (1.6124980948026215)

  var : num_white_odd
  2: 71966/98415 (0.7312503175328964)
  0: 26449/98415 (0.26874968246710357)
  mean: 143932/98415 (1.4625006350657928)

  var : num_blue_odd
  1: 71966/98415 (0.7312503175328964)
  3: 26449/98415 (0.26874968246710357)
  mean: 151313/98415 (1.5374993649342072)

  var : even
  (1 2): 26449/32805 (0.8062490474013108)
  (3 0): 6356/32805 (0.1937509525986892)
  (0 4): 0 (0.0)

  var : odd
  (2 1): 71966/98415 (0.7312503175328964)
  (0 3): 26449/98415 (0.26874968246710357)

  var : even_draws
  8: 1/5 (0.2)
  0: 1/5 (0.2)
  2: 1/5 (0.2)
  4: 1/5 (0.2)
  6: 1/5 (0.2)
  mean: 4 (4.0)

  var : odd_draws
  9: 1/5 (0.2)
  1: 1/5 (0.2)
  3: 1/5 (0.2)
  5: 1/5 (0.2)
  7: 1/5 (0.2)
  mean: 5 (5.0)


  This is a port of my WebPPL model ehrenfest_urn_scheme.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define (f n num_white num_blue)
     ;; Adjust for negative number of balls         
     (let ([num_white2 (if (<= num_white 0) 0 num_white)]
           [num_blue2  (if (<= num_blue 0)  0 num_blue)])
       (if (or (= n 0) (= (+ num_white2 num_blue2) 0))
           (list num_white2 num_blue2)
           (if (flip (/ num_white2 (+ num_white2 num_blue2)))
               ;; White ball
                   (f (sub1 n) (sub1 num_white2) (add1 num_blue2))
                   ;; Blue ball
                   (f (sub1 n) (add1 num_white2) (sub1 num_blue2))))))
   
    ;; Generate even and odd number of draws
    (define even_draws (* 2 (random-integer 5)))
    (define odd_draws (+ (* 2 (random-integer 5)) 1))

    (define white_init 1)
    (define blue_init 2)
    
    (define res_even (f even_draws white_init blue_init))
    (define num_white_even (first res_even))
    (define num_blue_even (second res_even))
    
    (define res_odd (f odd_draws white_init blue_init))
    (define num_white_odd (first res_odd))
    (define num_blue_odd (second res_odd))
    
    (list num_white_even
          num_blue_even
          num_white_odd
          num_blue_odd
          (list num_white_even num_blue_even)
          (list num_white_odd num_blue_odd)
          even_draws
          odd_draws
          )
    
   )
)

(show-marginals (model)
              (list "num_white_even"
                    "num_blue_even"
                    "num_white_odd"
                    "num_blue_odd"
                    "even"
                    "odd"
                    "even_draws"
                    "odd_draws"
                    )
              #:num-samples 1000
              #:truncate-output 5
              ; #:skip-marginals? #t
              ; #:show-stats? #t
              ; #:credible-interval 0.84
              ; #:show-histogram? #t
              ; #:show-percentiles? #t
              )


