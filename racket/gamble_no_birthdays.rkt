#| 

  No birthdays in Racket/Gamble 

  From John Allen Paulos
  https://twitter.com/JohnAllenPaulos/status/1584278111579181056
  """
  Randomly select 365 people and mark their birthdays on a calendar. 
  Days on which none of the 365 people have a birthday will remain 
  unmarked. Let U be the number of such unmarked days. Then 365/U is 
  approximately the transcendentally ubiquitous number e, 2.718 … Eerie! 
  """

  variable : p
  73/27: 0.07399999999999998
  73/26: 0.07099999999999998
  365/133: 0.07099999999999998
  365/134: 0.06399999999999999
  365/131: 0.061999999999999986
  ...
  365/148: 0.0019999999999999996
  73/30: 0.0009999999999999998
  365/153: 0.0009999999999999998
  365/152: 0.0009999999999999998
  365/151: 0.0009999999999999998
  mean: 2.730758581340858


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
   
   (define n 365)
   (define p (/ n (- n (length (remove-duplicates (for/list ([i n]) (random-integer n)))))))

   (list p)

   )
)

(show-marginals (model)
              (list  "p"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


