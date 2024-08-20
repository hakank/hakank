#| 

  Bayesian network in Racket Gamble.

  From https://dtai.cs.kuleuven.be/problog/tutorial/mpe/01_bn.html

  This is a port of my WebPPL model bayesian_network.wppl

  Output:

var : burglary
#t: 0.7900459418070443
#f: 0.20995405819295562
mean: 0.7900459418070443

var : earthquake
#f: 0.7914241960183768
#t: 0.20857580398162334
mean: 0.20857580398162334

var : alarm
#t: 0.7105666156202143
#f: 0.28943338437978566
mean: 0.7105666156202143

var : calls john
#f: 1.0
mean: 0 (0.0)

var : calls mary
#t: 1.0
mean: 1.0

var : calls bob
#t: 0.5973966309341501
#f: 0.4026033690658499
mean: 0.5973966309341501

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (bayesian-network)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define burglary (flip 0.7))
   (define earthquake (flip 0.2))
   (define alarm
     (cond
       [(and burglary earthquake) (flip 0.9)]
       [(and burglary (not earthquake)) (flip 0.8)]
       [(and (not burglary) earthquake) (flip 0.1)]
       [else #f]))
  
   ; Memoize
   (define calls
     (mem (lambda (x) (if alarm (flip 0.8) (flip 0.1)))))

   (observe/fail (eq? (calls "john") #f))
   (observe/fail (eq? (calls "mary") #t))

   ;; (case show-var
   ;;   [("burglary") burglary]
   ;;   [("earthquake") earthquake]
   ;;   [("alarm") alarm]
   ;;   [("calls john") (calls "john")]
   ;;   [("calls mary") (calls "mary")]
   ;;   [("calls bob") (calls "bob")]          
   ;;   )
   (list burglary earthquake alarm (calls "john") (calls "mary") (calls "bob"))
   )
  )

;; (for ([var '("burglary" "earthquake" "alarm" "calls john" "calls mary" "calls bob")])
;;   (show "variable" var)
;;   (let* ([model (bayesian-network var)])
;;     (show-model model)    
;;     (newline)
;;     )
;;   )

;; (newline)

(show-marginals (bayesian-network) '("burglary" "earthquake" "alarm" "calls john" "calls mary" "calls bob") )
