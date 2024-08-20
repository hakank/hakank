#| 

  From BayesiaLab, example Oil Rig in Racket Gamble.

  var : seismic_test
  #t: 1.0000000000000004
  mean: 1.0000000000000004

  var : test_cost
  -10: 1.0000000000000004
  mean: -10.000000000000004

  var : oil
  dry: 0.5000000000000001
  wet: 0.30000000000000004
  soaking: 0.20000000000000007

  var : test_result
  no_structure: 0.41000000000000014
  open_structure: 0.35000000000000003
  closed_structure: 0.24000000000000007

  var : drill
  #f: 0.5000000000000002
  #t: 0.5000000000000002
  mean: 0.5000000000000002

  var : gain
  0: 0.5000000000000002
  -70000: 0.25000000000000006
  50000: 0.15000000000000002
  200000: 0.10000000000000003
  mean: 10000.000000000004


  This model is a port of my WebPPL model oil_rig.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define structures (vector "closed_structure" "open_structure" "no_structure"))
    
   (define seismic_test (flip 0.5))
   (define test_cost (if seismic_test -10 0))
   (define oil (categorical-vw2 (vector 50 30 20) (vector "dry"  "wet" "soaking")))
    
   (define test_result 
     (if seismic_test
         (case oil
           [("dry") (categorical-vw2 (vector 10 30 60) structures)]
           [("wet") (categorical-vw2 (vector 30 40  30) structures)]
           [("soaking") (categorical-vw2 (vector 50 40 10) structures)])
         "test_result: na"
         ))
   
   (define drill (flip 0.5))
    
   (define gain 
     (if drill
         (case oil
           [("dry") -70000]
           [("wet") 50000]
           [("soaking") 200000])
         0))
            
    
    ;; (observe/fail (not drill))
    (observe/fail seismic_test)
    ;; (observe/fail (eq? oil "soaking")
    ;; (observe/fail (eq? test_result "open_structure"))
    ;; (observe/fail drill)
    
    (list seismic_test
          test_cost
          oil
          test_result
          drill
          gain
    )
 
   )
  )

(show-marginals (model)
                (list "seismic_test"
                      "test_cost"
                      "oil"
                      "test_result"
                      "drill"
                      "gain"
                 )
                #:num-samples 10000
                )


