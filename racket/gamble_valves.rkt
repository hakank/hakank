#| 

  Valves in Racket.Gamble 

  From BayesiaLabs' model Valves.xbl
  """
  Three valves are used to control the distribution of a fluid.

  Each valve has two failure modes, Sticks Closed (SC) and Sticks Opened (SO).

  The two failures rates of Valve1 are lower than those of Valve2, 
  and those of Valve2 are lower than those of Valve3
  """

  var : t
  10: 1.0000000000000002
  mean: 10.000000000000002

  var : (valve1 t)
  ok: 0.9660500000000002
  so: 0.022320000000000007
  sc: 0.011630000000000005

  var : (valve2 t)
  ok: 0.9655700000000002
  so: 0.022590000000000006
  sc: 0.011840000000000003

  var : (valve3 t)
  ok: 0.9633600000000002
  so: 0.023840000000000004
  sc: 0.012800000000000002

  var : (sticks_opened t)
  #f: 0.9990300000000004
  #t: 0.0009700000000000005
  mean: 0.0009700000000000005

  var : (sticks_closed t)
  #f: 0.9875700000000002
  #t: 0.012430000000000004
  mean: 0.012430000000000004

  var : (fluid_distribution t)
  #t: 0.9864000000000003
  #f: 0.013600000000000003
  mean: 0.9864000000000003


  This is a port of my WebPPL model valves.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate ; too slow
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define states (vector "ok" "so" "sc"))
   
   (define (valve1 t)
     (if (= t -1) 
         "ok"
         (let* ([t1 (sub1 t)]
                [valt1 (valve1 t1)])
           (case valt1
             [("ok") (categorical-vw2 (vector 99.7 0.2  0.1) states)]
             [("so") (categorical-vw2 (vector 0    100  0.1) states)]
             [("sc") (categorical-vw2 (vector 0    0    100) states)]))))
   
   (define (valve2 t)
     (if (= t -1) 
         "ok"
         (let* ([t1 (sub1 t)]            
                [valt1 (valve1 t1)])
           (case valt1
             [("ok") (categorical-vw2 (vector 99.5 0.3  0.2) states)]
             [("so") (categorical-vw2 (vector 0    100  0  ) states)]
             [("sc") (categorical-vw2 (vector 0    0    100) states)]))))
   
   (define (valve3 t)
     (if (= t -1) 
        "ok"
        (let* ([t1 (sub1 t)]
               [valt1 (valve1 t1)])
          (case valt1
            [("ok") (categorical-vw2 (vector 99.3 0.4 0.3) states)]
            [("so") (categorical-vw2 (vector 0    100 0  ) states)]
            [("sc") (categorical-vw2 (vector 0    0   100) states)]))))
   
   (define (sticks_opened t)
     (let ([valve1t (valve1 t)]
           [valve2t (valve2 t)]
           [valve3t (valve3 t)])
       (if [or
            (and (eq? valve1t "so") (eq? valve2t "ok") (eq? valve3t "so"))
            (and (eq? valve1t "so") (eq? valve2t "so"))
            (and (eq? valve1t "so") (eq? valve2t "sc") (eq? valve3t "so"))
            (and (eq? valve1t "so") (eq? valve2t "sc") (eq? valve3t "so"))]
           #t
           #f)))
   
   (define (sticks_closed t)
     (let ([valve1t (valve1 t)]
           [valve2t (valve2 t)]
           [valve3t (valve3 t)])
       (if (or
            
            (and (eq? valve1t "ok") (eq? valve2t "ok") (eq? valve3t "sc"))
            (and (eq? valve1t "ok") (eq? valve2t "so") (eq? valve3t "sc"))
            (and (eq? valve1t "ok") (eq? valve2t "sc") (eq? valve3t "sc"))
            
            (and (eq? valve1t "so") (eq? valve2t "ok") (eq? valve3t "sc"))
            (and (eq? valve1t "so") (eq? valve2t "so") (eq? valve3t "sc"))
            (and (eq? valve1t "so") (eq? valve2t "sc") (eq? valve3t "sc"))
            
            (and (eq? valve1t "sc") (eq? valve2t "ok") (eq? valve3t "sc"))
            (and (eq? valve1t "sc") (eq? valve2t "so") (eq? valve3t "sc"))
            (and (eq? valve1t "sc") (eq? valve2t "sc") (eq? valve3t "sc"))
            (and (eq? valve1t "sc") (eq? valve2t "sc")))
           #t
           #f)))
   
   (define (fluid_distribution t)
     (if (and (eq? (sticks_opened t) #f) (eq? (sticks_closed t) #f))
         #t
         #f))
   
   (define t 10)
  
   (list t
         (valve1 t)
         (valve2 t)
         (valve3 t)
         
         (sticks_opened t)
         (sticks_closed t)
         (fluid_distribution t)
         )

   )
)

(show-marginals (model)
              (list  "t"
                     "(valve1 t)"
                     "(valve2 t)"
                     "(valve3 t)"
                     
                     "(sticks_opened t)"
                     "(sticks_closed t)"
                     "(fluid_distribution t)"
                     )
              #:num-samples 100000
              ; #:truncate-output 5
              ; #:skip-marginals? #t
              ; #:show-stats? #t
              ; #:credible-interval 0.84
              ; #:show-histogram? #t
              ; #:show-percentiles? #t
              )


