#| 

  Urn drawing in Racket Gamble.

  Port of the Church model in 
  Stuhlmuller & Goodman: "Reasoning about Reasoning by Nested Conditioning:
                          Modeling Theory of Mind with Probabilistic Programs"
  (https://stuhlmueller.org/papers/nested-conditioning-cogsys2013.pdf)


  Using importance-sampler

var : urn->proportion-red 3
0.9997754546520097: 0.0009999999999999994
0.12550984869457749: 0.0009999999999999994
0.9969407573358617: 0.0009999999999999994
0.9137126479836877: 0.0009999999999999994
0.9657205764271052: 0.0009999999999999994
0.646858128697988: 0.0009999999999999994
...
0.9554097561695171: 0.0009999999999999994
0.8318476185627586: 0.0009999999999999994
0.6606087658553098: 0.0009999999999999994
0.9770813883493665: 0.0009999999999999994
0.5553425909447453: 0.0009999999999999994
0.5307861107056701: 0.0009999999999999994
mean: 0.7310039518629464
Min: 0.0007247563798591719 Mean: 0.7359272002248616 Max: 0.9999982206410307 Variance: 0.05644248939763439 Stddev: 0.23757628121854754
Credible interval (0.84): 0.489513879370672..0.9999982206410307

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (urn-drawing)
  
  (; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; model
   (define bias (uniform 0 10) )
   (define red-bias (uniform 0 bias ) )
   (define black-bias (- bias red-bias ) )
   (define urn->proportion-red
            (mem
              (lambda ( urn )
                (beta (+ .4 red-bias ) (+ .4 black-bias)))))
                ; (first (beta (+ .4 red-bias ) (+ .4 black-bias))))))   

   (define (sample-urn urn )
            ( if (flip ( urn->proportion-red urn ) )
                 'red
                 'black ) )
   ;; condition
   (observe/fail (equal? ( repeat (lambda () (sample-urn 1) ) 15)
                         '(red red red red red red red red
                               red red red red red red black ) ) )
   
   ;; query expression
   (list (urn->proportion-red 3))
  )
  )

(show-marginals (urn-drawing)
                (list "urn->proportion-red 3")
                #:truncate-output 6
                #:show-stats? #t
                #:credible-interval 0.84
                )


