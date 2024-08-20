#| 

  ClickGraph example from PSI I paper in Racket Gamble.

  Where the objective is to determine how similar the 
  clicks of A and B are.

  Very similar clicks:

(clicks ((#t #t #t #f #f) (#t #t #t #f #f)))
var : simAll
0.7208903790323993: 0.16199999999999998
0.7312315528039269: 0.137
0.895040959391864: 0.127
0.3778120224794608: 0.08
0.6110132299574912: 0.069
...
0.6154590807891193: 0.013000000000000001
0.19343482429032297: 0.009
0.5906997159741681: 0.005
0.875911348077832: 0.003
0.7428466345910216: 0.001
mean: 0.6317704505094451

var : sim 0
#t: 0.673
#f: 0.32700000000000007
mean: 0.673

  Very different clicks

(clicks ((#t #t #t #f #t #t #t #f) (#f #f #f #t #f #f #f #t)))
var : simAll
0.46735560316824487: 0.20800000000000002
0.10469167231951558: 0.197
0.058087573871532315: 0.143
0.15656756203762556: 0.134
0.19880573320006778: 0.074
...
0.07107103005581868: 0.035
0.4228107051329284: 0.031
0.4664906954928452: 0.026
0.3291974336544672: 0.018
0.08539703855351173: 0.001
mean: 0.250772159448501

var : sim 0
#f: 0.8930000000000001
#t: 0.107
mean: 0.107



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (click-graph clicks)

  (define clicksAs (first clicks))
  (define clicksBs (second clicks))
  (define len (length clicksAs))
  
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler
       
    (define simAll (uniform 0 1))
    
    (define sim (mem (lambda (i) 
                       (flip simAll))))
    
    (define pA (mem (lambda (i) 
        (uniform 0 1))))
    
    (define pB (mem (lambda (i) 
        (if (sim i)
            (pA i)
            (uniform 0 1)))))
    
    (define clicksA (mem (lambda (i)
        (flip (pA i)))))

    (define clicksB (mem (lambda (i)
        (flip (pB i)))))

    (for ([i (range len)])
      (observe/fail (eq? (clicksA i) (list-ref clicksAs i)))
      (observe/fail (eq? (clicksB i) (list-ref clicksBs i))))

    (list  simAll (sim 0))

   )
  )

;; Very similar
(define clicks1 (list '(#t #t #t #f #f)
                      '(#t #t #t #f #f)))
       
;; Variant: completely different
(define clicks2 (list
                 '(#t #t #t #f #t #t #t #f)
                 '(#f #f #f #t #f #f #f #t)))


(for ([clicks (list clicks1 clicks2)])
  (displayln (list "clicks" clicks))
  (show-marginals (click-graph clicks)
                  (list "simAll" "sim 0")
                  #:truncate-output 5
                  )
  )
