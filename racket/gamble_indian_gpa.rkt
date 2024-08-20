#| 

  Indian GPA problem Racket Gamble.

  From cplint http://cplint.ml.unife.it/example/inference/indian_gpa.pl
  """
  The Indian GPA Problem. From 
  http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=indian-gpa 
  "This example was inspired by Stuart Russell...the problem is: if you observe 
  that a student GPA is exactly 4.0 in a model of transcripts of students 
  from the USA (GPA's from 0.0 to 4.0 and India (GPA's from 0.0 to 
  10.0) what is the probability that the student is from India?... 
  As we know from statistics, given the mixture distribution and given the 
  fact that his/her GPA is exactly 4.0, the probability that the student 
  is American must be 1.0.
  (i.e. zero probability that the student is from India)."
  Probabilistic logic program from 
  https://github.com/davidenitti/DC/blob/master/examples/indian-gpa.pl
  """

  * observe gpa 4.0

  var : nationality
  usa: 0.9999899975994239
  india: 1.0002400576138273e-5

  var : perfect
  #t: 0.9999399855965432
  #f: 6.001440345682964e-5
  mean: 0.9999399855965432

  var : gpa
  4.0: 0.9999399855965432
  2.0896036286460133: 2.5006001440345684e-5
  1.7087476224218276: 2.5006001440345684e-5
  1.598890144976124: 1.0002400576138273e-5
  mean: 3.999870916702735

  * observe gpa 3.0

  var : nationality
  usa: 0.7068816585084963
  india: 0.29311834149152916

  var : perfect
  #f: 0.9999999999999795
  mean: 0 (0.0)

  var : gpa
  2.793981980799756: 0.00014396775122374082
  3.9114985274131633: 0.00014396775122374082
  0.6273421017656003: 0.00014396775122374082
  2.2984591541065615: 0.00014396775122374082
  ...
  4.90348670862737: 5.758710048949634e-5
  9.963627087984802: 5.758710048949634e-5
  8.920878969492119: 5.758710048949634e-5
  9.7929824369355: 5.758710048949634e-5
  mean: 2.898738625202677

  * observe perfect

  var : nationality
  usa: 0.6009
  india: 0.3991

  var : perfect
  #t: 1.0
  mean: 1.0

  var : gpa
  4.0: 0.6009
  10.0: 0.3991
  mean: 6.3946000000000005


  This is a port of my WebPPL model indian_gpa.wppl
  which a port of my Turing.jl model indian_gpa.jl (inspired by the SPP model indian-pga.py).


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define nationality (categorical-vw2 (vector 0.5 0.5) (vector "india" "usa")))
   ;; Note: In https://probprog.github.io/anglican/examples/viewer/?worksheet=indian-gpa
   ;;       the proportions are a little different.
   ;; (define nationality (categorical-vw2 (vector 0.75 0.25) (vector "india" "usa")))
   
   
   (define perfect (if (eq? nationality "india") (flip 0.10) (flip 0.15)))
   (define gpa (if (eq? nationality "india")
                   (if perfect
                       (dist-unit 10.0)
                       (uniform-dist 0 10))
                   (if perfect
                       (dist-unit 4.0)
                       (uniform-dist 0 4))))


   ; (observe-sample gpa 4.0)  
   ; (observe-sample gpa 3.0)
   ; (observe-sample gpa 4.1) 
   (observe/fail perfect)
   ; (observe/fail (not perfect))
   ; (observe/fail (not (eq? gpa 4.0)))
   ; (observe/fail (eq? nationality "usa"))
   
   (list
        nationality
        perfect
        (sample gpa)
        )

   )
  )

(show-marginals (model)
                  (list "nationality"
                        "perfect"
                        "gpa"
                        )
                  #:num-samples 10000
                  #:truncate-output 4
                  ; #:skip-marginals? #t
                  ; #:show-histogram? #t
                  )
