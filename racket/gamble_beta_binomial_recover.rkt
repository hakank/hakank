#| 

  Beta binomial recovering parameters in Racket.Gamble 

  Data and parameters from
  https://en.wikipedia.org/wiki/Beta-binomial_distribution#Example

  var : x
  7: 0.223
  6: 0.201
  5: 0.156
  8: 0.145
  4: 0.108
  ...
  3: 0.036
  10: 0.027
  2: 0.019
  1: 0.008
  11: 0.004
  mean: 6.263999999999999
  Credible interval (0.84): 3..8
  Percentiles:
  (0.01 2)
  (0.025 3)
  (0.1 4)
  (0.05 3)
  (0.25 5)
  (0.5 6)
  (0.75 8)
  (0.84 8)
  (0.9 9)
  (0.95 9)
  (0.975 10)
  (0.99 10)
  (0.999 11)
  Histogram:
   1: 5  
   2: 18 
   3: 50 
   4: 80 
   5: 151
   6: 212
   7: 226
   8: 144
   9: 79 
  10: 29 
  11: 6  


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define males    '(0  1   2   3   4    5   6     7   8   9  10 11 12))
(define families '(3 24 104 286 670 1033 1343 1112 829 478 181 45  7))
(define num_families (sum families))
(define families_pct (map (lambda (v) (/ v num_families)) families))
(show2 "families:" families)

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define a 34.09558) ;; 34.1350   
   (define b 31.5715) ;; 31.6085   
   (define x (beta_binomial 12 a b))
    
   (list x)
    
   )
)

(show-marginals (model)
              (list  "x"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.84
                    #:show-histogram? #t
                    #:show-percentiles? #t
                    )


#|
  Compare data with the result of the model:

  (hist: (3 19 105 293 681 1049 1260 1173 845 472 175 36 4))
  (males: 0 data: 3 est: 3 diff: 0)
  (males: 1 data: 24 est: 19 diff: 5)
  (males: 2 data: 104 est: 105 diff: -1)
  (males: 3 data: 286 est: 293 diff: -7)
  (males: 4 data: 670 est: 681 diff: -11)
  (males: 5 data: 1033 est: 1049 diff: -16)
  (males: 6 data: 1343 est: 1260 diff: 83)
  (males: 7 data: 1112 est: 1173 diff: -61)
  (males: 8 data: 829 est: 845 diff: -16)
  (males: 9 data: 478 est: 472 diff: 6)
  (males: 10 data: 181 est: 175 diff: 6)
  (males: 11 data: 45 est: 36 diff: 9)
  (males: 12 data: 7 est: 4 diff: 3)
  total diff: 224

  Compare with the table at the Wikipedia page:
  Males	   0    1     2     3     4      5      6      7     8     9   10    11   12
  Observed 3   24   104   286   670   1033   1343   1112   829   478   181   45   7
  Fitted   2.3 22.6 104.8 310.9 655.7 1036.2 1257.9 1182.1 853.6 461.9 177.9 43.8 5.2

  This
  model    3   19   105   293   681   1049   1260   1173   845   472   175   36   4

|#

(define samples (map (lambda (v) (first v)) (make-samples (model) num_families)))
(define hist (vector->list (first (histogram samples (length families)))))
(define num_hist (sum hist))
(define hist_pct (map (lambda (v) (/ v num_hist)) hist))
(show2 "hist:" (map (lambda (v) (* v num_families)) hist_pct))
(define total_diff 0)
(for ([i 13])
  (let* ([f (list-ref families i)]
         ; [est (round (* (list-ref hist_pct i) num_families))]
         [est (* (list-ref hist_pct i) num_families)]
         [diff (- f est)])
    (show2 "males:" i "data:" f "est:" est "diff:" diff)
    (set! total_diff (+ total_diff (abs diff)))
    )
  )
(show "total diff" total_diff)
(newline)


#|

% This does not work, too slow!
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n (add1 (random-integer 50)))
   (define a (add1 (random-integer 50)))
   (define b (add1 (random-integer 20)))
   
   (for ((v samples))
     ; (observe-sample (dist-unit (beta_binomial n a b)) v) 
     (observe/fail (<= (abs (- (beta_binomial n a b) v)) 1))
     )

   (define post (beta_binomial n a b))
   (show2 "n" n "a" a "b" b "post" post)
   
   (list n
         a
         b
         post)
    
   )
)

(show-marginals (model2)
                (list  "n"
                       "a"
                       "b"
                       "post"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.84
                    ; #:show-histogram? #t
                    #:show-percentiles? #t
                    )
|#

