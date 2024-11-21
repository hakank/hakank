#| 

  Hypergeometric2 recovering parameters in Racket/Gamble 

  Let's check for num_special with fixed num_draws=10 num_total=100

  * Fixed num_draws=10 num_total=100, num_special is free (1..100)

  (num_sims: 100 mean: 4.06)
  var : num_draws
  10: 1.0000000000000002
  mean: 10.000000000000002

  var : num_special
  41: 0.26921946910071687
  39: 0.14498089032506348
  40: 0.1150100709813564
  38: 0.09391807845265251
  36: 0.0743721653409798
  ...
  96: 6.160073044081835e-70
  97: 1.6205637877930946e-72
  98: 1.8312133053348065e-75
  99: 1.1401421126978942e-79
  100: 2.2563524575315077e-82
  mean: 39.8277770105376
  HPD interval (0.84): 37..42

  var : num_total
  100: 1.0000000000000002
  mean: 100.00000000000003

  var : post
  3: 0.38908676945794374
  2: 0.3687768226502937
  5: 0.1276833679201215
  4: 0.07706163265643266
  6: 0.02114653239291199
  ...
  0: 0.0032835411155080815
  7: 0.002858668166009952
  10: 0.0005233466527672627
  8: 0.00012730314851419812
  9: 7.7486647318291e-6
  mean: 3.014133092293436
  HPD interval (0.84): 2..6

  * If we let all parameters be free and restrict them to 1..100:
  var : num_draws
  mean: 27.29673642742986
  HPD interval (0.84): 4..41

  var : num_special
  mean: 29.898740004615693
  HPD interval (0.84): 4..54

  var : num_total
  mean: 59.86653909927507
  HPD interval (0.84): 26..56

  var : post
  mean: 4.108166253017906
  HPD interval (0.84): 4..4


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

;; Let's simulate the following:
;; We have 100 different objects of which 40 are special.
;; We now draw 10 objects. How many of these are special?
(define num_sims 100)
(define data (repeat (lambda () (hypergeometric2 10 40 100)) num_sims))
(show2 "num_sims:" num_sims "mean:" (* 1.0 (avg data)))
data

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define num_draws 10)
   ; (define num_draws (add1 (random-integer 100)))
   (define num_special (add1 (random-integer 100)))
   (define num_total 100)
   ; (define num_total (add1 (random-integer 100)))

   ;; For letting all parameters free
   (observe/fail (>= num_total num_draws))
   (observe/fail (>= num_total num_special))
   
   (for ([i num_sims])
     (observe-sample
      (normal-dist (hypergeometric2 num_draws num_special num_total) 3)
      (list-ref data i)))
   
   (define post (hypergeometric2 num_draws  num_special  num_total))
   
   (list num_draws
         num_special
         num_total
         post
         )
   
   )
)

(show-marginals (model)
                (list  "num_draws"
                       "num_special"
                       "num_total"
                       "post"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


