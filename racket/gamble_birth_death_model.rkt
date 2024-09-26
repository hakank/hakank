#| 

  Birth death model in Racket Gamble.

  From https://www.nature.com/articles/s42003-021-01753-7
  Basic birth-death model simulation in WebPPL

var : lambda
0.43562479569856627: 0.0009999999999999994
0.81776169699517: 0.0009999999999999994
1.1885698871065409: 0.0009999999999999994
0.7143919354154178: 0.0009999999999999994
1.2870454190494394: 0.0009999999999999994
...
1.2189334376172964: 0.0009999999999999994
1.3046765190429874: 0.0009999999999999994
1.5819983365252164: 0.0009999999999999994
0.6009537255885126: 0.0009999999999999994
0.45779282690017226: 0.0009999999999999994
mean: 1.63256877074692
Credible interval (0.84): 0.15155048990670164..2.6170808384060917

var : mu
0.008784452845391146: 0.0009999999999999994
0.4990576379740192: 0.0009999999999999994
0.5367237561808959: 0.0009999999999999994
0.0066245981904169806: 0.0009999999999999994
1.406358356185718: 0.0009999999999999994
...
1.872915288989987: 0.0009999999999999994
0.6426525029268354: 0.0009999999999999994
0.11236097911422027: 0.0009999999999999994
0.11736817209511004: 0.0009999999999999994
0.7082654413527031: 0.0009999999999999994
mean: 0.5115528797354453
Credible interval (0.84): 0.00015921272945784636..0.8649895421429526


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (goesExtinct time_ lambda_ mu) 
  (define waitingTime (exponential (+ lambda_ mu)))  
  (define isSpeciation (flip (/ lambda_ (+ lambda_ mu))))
  
  (if (> waitingTime time_)
      #f
      (if (not isSpeciation)
          #t
          (and (goesExtinct (- time_ waitingTime) lambda_ mu)
               (goesExtinct (- time_ waitingTime) lambda_ mu))))
)


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define lambda_ (gamma 1 1))
   (define mu (gamma 1 1))
   
   ;; (define t = 10
   (define t 20)
   
   (observe/fail (not (goesExtinct t lambda_ mu)))
   
   (list lambda_
         mu
         )
   
   )
  )


(show-marginals (model)
                      (list  "lambda"
                             "mu"
                             )
                      #:num-samples 1000
                      #:truncate-output 5
                      ; #:skip-marginals? #t
                      ; #:show-stats? #t
                      #:credible-interval 0.84
                      ; #:show-histogram? #t
                      ; #:show-percentiles? #t
                      )


