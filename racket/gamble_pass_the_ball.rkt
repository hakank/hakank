#| 

  Pass the Ball in Racket/Gamble 

  From https://www.quantguide.io/questions/pass-the-ball
  """ 
  You and 4 other people are sitting in a circle. You are given a ball to start the game. 
  Every second of this game, the person with the ball has three choices they can make. 
  They can either pass the ball to the left, pass the ball to the right, or keep the ball 
  (all with equal probability). This game goes on till someone keeps the ball. What is the 
  probability that you are the person to end the game and keep the ball?
  """

  Via Pascal Berker: https://medium.com/@pbercker/pass-the-ball-a-probability-puzzle-with-a-markov-chain-and-a-dbn-3c170a988cba

  The probability is 5/11: 0.45454545454545453


  Using discrete_markov_process_absorption, showing the probabilities 
  of holding the ball (and ending the game) for each person:
  (5/11 2/11 1/11 1/11 2/11)
  
  Expected number of passes:
  (total-passes: 2)

  The absorption probability matrix:
  discrete_markov_process_absorption probability matrix
  (5/11 2/11 1/11 1/11 2/11)
  (2/11 5/11 2/11 1/11 1/11)
  (1/11 2/11 5/11 2/11 1/11)
  (1/11 1/11 2/11 5/11 2/11)
  (2/11 1/11 1/11 2/11 5/11)


  Fundamental matrix (passing times)
  discrete_markov_process_absorption fundamental matrix
  (15/11 6/11 3/11 3/11 6/11)
  (6/11 15/11 6/11 3/11 3/11)
  (3/11 6/11 15/11 6/11 3/11)
  (3/11 3/11 6/11 15/11 6/11)
  (6/11 3/11 3/11 6/11 15/11)



  Using the simulation model.

 * Exact probability (enumerate), with time limit 20

variable : pos
1: 176100749/387420489 (0.45454681412061304)
2: 633956965/3486784401 (0.1818170818987784)
5: 633956965/3486784401 (0.1818170818987784)
3: 316981865/3486784401 (0.09090951104091509)
4: 316981865/3486784401 (0.09090951104091509)
mean: 915719839/387420489 (2.3636329646984673)

variable : c
0: 1/3 (0.3333333333333333)
1: 2/9 (0.2222222222222222)
2: 4/27 (0.14814814814814814)
3: 8/81 (0.09876543209876543)
4: 16/243 (0.06584362139917696)
5: 32/729 (0.0438957475994513)
6: 64/2187 (0.029263831732967534)
7: 128/6561 (0.01950922115531169)
8: 256/19683 (0.01300614743687446)
9: 512/59049 (0.008670764957916306)
10: 1024/177147 (0.0057805099719442045)
11: 2048/531441 (0.0038536733146294698)
12: 4096/1594323 (0.002569115543086313)
13: 8192/4782969 (0.0017127436953908754)
14: 16384/14348907 (0.0011418291302605836)
15: 32768/43046721 (0.0007612194201737224)
16: 65536/129140163 (0.0005074796134491483)
17: 131072/387420489 (0.00033831974229943216)
20: 1048576/3486784401 (0.0003007286598217175)
18: 262144/1162261467 (0.0002255464948662881)
19: 524288/3486784401 (0.00015036432991085875)
mean: 6971471650/3486784401 (1.9993985426803567)

variable : p
#f: 211319740/387420489 (0.545453185879387)
#t: 176100749/387420489 (0.45454681412061304)
mean: 176100749/387420489 (0.45454681412061304)


  * Using importance-sampler (no time limit), 1000000 samples
variable : pos
1: 0.4550489999999998
5: 0.18179599999999999
2: 0.181053
4: 0.091359
3: 0.09074299999999998
mean: 2.3637999999999995

variable : c
0: 0.33453199999999994
1: 0.22154999999999997
2: 0.14754599999999995
3: 0.09880199999999997
4: 0.06580699999999998
5: 0.043856999999999986
6: 0.02943999999999999
7: 0.019506999999999997
8: 0.013039999999999996
9: 0.008684999999999998
10: 0.005859999999999999
11: 0.0038579999999999986
12: 0.0025139999999999997
13: 0.0017109999999999998
14: 0.0011389999999999996
15: 0.0007289999999999998
16: 0.0004559999999999999
17: 0.00031199999999999994
18: 0.00023399999999999997
19: 0.00013399999999999998
20: 0.00010599999999999999
21: 4.7999999999999994e-5
22: 4.3999999999999985e-5
23: 2.4999999999999994e-5
24: 1.5999999999999996e-5
25: 1.2999999999999998e-5
26: 9.999999999999997e-6
27: 8.999999999999999e-6
29: 5.9999999999999985e-6
28: 1.9999999999999995e-6
30: 1.9999999999999995e-6
31: 1.9999999999999995e-6
32: 1.9999999999999995e-6
35: 9.999999999999997e-7
36: 9.999999999999997e-7
mean: 1.9972929999999989

variable : p
#f: 0.5449510000000001
#t: 0.4550489999999998
mean: 0.4550489999999998


  


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")
(require math/matrix) ; exact rationals supported


; Circle
;          1
;       5      2
;         4   3
;        
;  1 -> 2, 1 -> 5
;  2 -> 1, 2 -> 3
;  3 -> 2, 3 -> 4
;  4 -> 3, 4 -> 5
;  5 -> 1, 5 -> 4
(define transition '((1/3 1/3   0   0 1/3)
                     (1/3 1/3 1/3   0   0)
                     (  0 1/3 1/3 1/3   0)
                     (  0   0 1/3 1/3 1/3)
                     (1/3   0   0 1/3 1/3)))
; (define init (append '(1) (rep 4 0)))



(show2 "discrete_markov_process_absorption:" (discrete_markov_process_absorption_probabilities transition 0))
(newline)
(show2 "total-passes:" (discrete_markov_process_absorption_total_passes transition 0))
(newline)
(display-matrix (discrete_markov_process_absorption_probability_matrix transition 0) "discrete_markov_process_absorption probability matrix")
(newline)
(display-matrix (discrete_markov_process_absorption_fundamental_matrix transition 0) "discrete_markov_process_absorption fundamental matrix")
(newline)


#|
  Simulation.

  Note: Enumeration requires a time limit, otherwise it will calculate *all*
        probabilities (until "infinity").

|#
(define (model)
  (enumerate ; #:limit 1e-03
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 5)

   (define t 20)

   ; With time limit (t)
   (define (f pos c)
     (if (>= c t)
         (list pos c)
         (let ([new-pos (uniform-draw (for/list ([v '(-1 0 1)])
                                        (modulo (+ pos v) n)))])
           ; End if a person get the ball and decide to keep it
           (if (= new-pos pos)
               (list pos c)
               (f new-pos (+ c 1))))
     ))

   ;; Without time limit. Works with enumerate with #:limit or importance-sample
   ;; (define (f pos c)
   ;;   (let ([new-pos (uniform-draw (for/list ([v '(-1 0 1)])
   ;;                                  (modulo (+ pos v) n)))])
   ;;     ; End if a person get the ball and decide to keep it
   ;;     (if (= new-pos pos)
   ;;         (list pos c)
   ;;         (f new-pos (+ c 1))))
   ;;   )

   (define init-state 0)
   (define init-step 0)
   (define a (f init-state init-step))
   (define pos (first a))
   (define c (second a))
   (define p (= pos init-state))

   (list pos c p)
   
   )
)

(show-marginals (model)
                (list  "pos"
                       "c"
                       "p"
                       )
                #:num-samples 1000000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
              )



#|
  This model is inspired by Pascal's Netica model in his Medium post (see above).
 
  States 0..4: Each person's state
  States 5..9: Each person's absorption state

  discrete_markov_process_absorption_probabilities:
  '(0 0 0 0 0 5/11 2/11 1/11 1/11 2/11)


  Using the importance-sampler (1 000 000 samples):

variable : pos
5: 0.4556
6: 0.18278999999999998
9: 0.18076999999999993
8: 0.09042999999999998
7: 0.09040999999999998
mean: 6.357979999999999

variable : c
1: 0.3343499999999999
2: 0.22425999999999996
3: 0.14620999999999998
4: 0.09759999999999998
5: 0.06557999999999999
6: 0.04353999999999999
7: 0.02969999999999999
8: 0.019459999999999998
9: 0.012909999999999998
10: 0.008799999999999999
11: 0.005999999999999998
12: 0.0035499999999999993
13: 0.0025999999999999994
14: 0.0016899999999999997
15: 0.0011699999999999998
16: 0.0008299999999999998
17: 0.0005999999999999998
18: 0.0004599999999999999
19: 0.00021999999999999995
21: 0.00014
20: 0.00013
22: 7e-5
23: 3.9999999999999996e-5
24: 2.9999999999999997e-5
32: 1.9999999999999998e-5
25: 1.9999999999999998e-5
33: 9.999999999999999e-6
28: 9.999999999999999e-6
mean: 2.9988299999999994

variable : p
#f: 0.5444000000000001
#t: 0.4556
mean: 0.4556


|#


;                   States               Absorption states
;                   0   1   2   3   4     5   6   7   8   9
(define probs2 '( (0/3 1/3 0/3 0/3 1/3   1/3 0/3 0/3 0/3 0/3 )
                  (1/3 0/3 1/3 0/3 0/3   0/3 1/3 0/3 0/3 0/3 )
                  (0/3 1/3 0/3 1/3 0/3   0/3 0/3 1/3 0/3 0/3 )
                  (0/3 0/3 1/3 0/3 1/3   0/3 0/3 0/3 1/3 0/3 )
                  (1/3 0/3 0/3 1/3 0/3   0/3 0/3 0/3 0/3 1/3 )
                  
                  (0/3 0/3 0/3 0/3 0/3   3/3 0/3 0/3 0/3 0/3 )
                  (0/3 0/3 0/3 0/3 0/3   0/3 3/3 0/3 0/3 0/3 )
                  (0/3 0/3 0/3 0/3 0/3   0/3 0/3 3/3 0/3 0/3 )
                  (0/3 0/3 0/3 0/3 0/3   0/3 0/3 0/3 3/3 0/3 )
                  (0/3 0/3 0/3 0/3 0/3   0/3 0/3 0/3 0/3 3/3 )))

(discrete_markov_process_absorption_probabilities probs2 0)

(newline)

(define (model2 [use-time-limit? #f])
  (; enumerate #:limit 1e-03
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define t 20)
   (define init-state 0)
   (define init-step 0)

   (define mat probs2)
   (define mat-lan (length mat))
   
   (define (f state c)
     (if (and use-time-limit? (> c t))
         (list state c)
         (let* ([new-state (categorical-vw2 (list->vector (list-ref mat state)) (list->vector (range 10)))]
                [v (list-ref2d mat state new-state)]
                )
           (if (= v 1)
               (list new-state c)
               (f new-state (add1 c))
     ))))

   (define a (f init-state init-step))
   (define pos (first a))
   (define c (second a))
   (define p (= pos (+ 5 init-state)))
       
   (list pos c p)
   
   )
)

#|
(show-marginals (model2 #f)
                (list  "pos"
                       "c"
                       "p"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
              )
|#
