#| 

  Sum Pareto in Racket.Gamble 

  From PSI test/approximate/sumpareto.psi
  """
  def main() (
    s:=0;
    for i in (0..2) (
        x:=pareto(1.161,1); //80-20 rule: https://en.wikipedia.org/wiki/Pareto_distribution#Relation_to_the_.22Pareto_principle.22
		s = s + x;
    ) ;; mathematica couldn't solve this integral even after 5 days. 
    return s;
  )
  ;; E(s) = 2322/161
  ;; (14.4223602484472)
  """  

var : p
3.638324355840224: 0.00010000000000000938
15.737862333907348: 0.00010000000000000938
4.4066229700213455: 0.00010000000000000938
3.3188213602696477: 0.00010000000000000938
4.605557668804047: 0.00010000000000000938
...
2.4612236380926493: 0.00010000000000000938
8.776810052361622: 0.00010000000000000938
7.3536169619944: 0.00010000000000000938
2.2491041045874263: 0.00010000000000000938
4.784578694034191: 0.00010000000000000938
mean: 12.685840764936751
Min: 2.003646723189892 Mean: 10.528118897204871 Max: 3481.3778687217637 Variance: 3376.826188132008 Stddev: 58.11046539249198

var : p2
49.98344408059221: 0.00010000000000000938
3.348493490266035: 0.00010000000000000938
30.962499263907524: 0.00010000000000000938
4.060891857025448: 0.00010000000000000938
31.502899855793203: 0.00010000000000000938
...
5.058137113680574: 0.00010000000000000938
6.108497290304156: 0.00010000000000000938
3.403266595891163: 0.00010000000000000938
4.529849786856164: 0.00010000000000000938
4.6003559279739745: 0.00010000000000000938
mean: 10.818646913284764
Min: 2.0101652813097877 Mean: 10.935965646075937 Max: 6022.673065038591 Variance: 6725.530768164448 Stddev: 82.00933342104695



  This is a port of my WebPPL model sum_pareto.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;; From 
;; https://math.stackexchange.com/questions/1777367/how-to-generate-a-random-number-from-a-pareto-distribution
;; Shape a, Scale b:
(define (my-pareto shape scale)
  (let ([u (uniform 0 1)])
    (/ scale (expt (- 1 u) (/ 1 shape)))))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; 80-20 rule: https://en.wikipedia.org/wiki/Pareto_distribution#Relation_to_the_.22Pareto_principle.22
   ; (defmem (f i) (pareto 1.161 1)) ; Gamble: (pareto scale shape)
   (defmem (f i) (pareto 1 1.161))   

   (defmem (f2 i) (my-pareto 1.161 1)) ; here shape scale
   
   ; (define p (for/sum ((i 2)) (pareto 1.161 1)))
   (define p (for/sum ((i 2)) (f i)))
   (define p2 (for/sum ((i 2)) (f2 i)))   

   (list p
         p2
         )

   )
)

(show-marginals (model)
                (list  "p"
                       "p2"
                     )
              #:num-samples 10000
              #:truncate-output 5
              ; #:skip-marginals? #t
              #:show-stats? #t
              ; #:credible-interval 0.84
              ; #:show-histogram? #t
              ; #:show-percentiles? #t
              )


