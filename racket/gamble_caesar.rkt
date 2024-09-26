#| 

  Simple Caear cipher in Racket Gamble.

  Port of Dice model
  https://github.com/SHoltzen/dice/blob/master/benchmarks/simpleCaesar.dice

Model 1:
var : key1
2: 0.629
0: 0.255
1: 0.116
mean: 1.374
Credible interval (0.84): 0..2

var : observation
2: 1.0
mean: 2.0
Credible interval (0.84): 2..2

Model 2:
reps: 1
var : key
3: 0.486
2: 0.271
1: 0.135
0: 0.108
mean: 2.135

reps: 2
var : key
3: 0.719
2: 0.186
0: 0.051
1: 0.044
mean: 2.573

reps: 3
var : key
3: 0.858
2: 0.117
0: 0.015
1: 0.01
mean: 2.8179999999999996

reps: 4
var : key
3: 0.933
2: 0.06
0: 0.005
1: 0.002
mean: 2.9210000000000003

  This is a port of my WebPPL model caesar.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (priors vs) (for/list ((i (length vs))) (cons i (list-ref vs i))))

(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define (drawChar key observation)
        
     ;; (define s = (
     ;;     0.00125 0.06125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125
     ;;     0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125
     ;;     0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125
     ;;     0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125)
     (define s '(
                 0.08167 0.01492 0.02782 0.04253 0.04378 0.02228 0.02015 0.06094
                 0.06966 0.0153  0.0772 0.04025 0.02406 0.06749 0.07507 0.01929
                 0.00095 0.05987 0.06327 0.09056 0.02758 0.00978 0.02360 0.00150
                 0.01974 0.00074))
     (define drawnChar (discrete (priors s)))

     (define len (length s))
     (define encrypted (+ key drawnChar))
     ;; (define encrypted = (key + drawnChar) % len ;; hakank: Shouldn't it be modulo len?
     (observe/fail (= observation encrypted))
     
     encrypted
     )
   
   (define key1 (discrete (priors '(0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125
                                            0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125
                                            0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125
                                            0.03125 0.03125 0.03125 0.03125 0.03125 0.03125 0.03125
                                            0.03125 0.03125 0.03125 0.03125))))
   ;; We only do one observation here
   (define observation (drawChar key1 2))

   (list key1
         observation
    )
  
   )
  )

(displayln "Model 1:")
(show-marginals (model1)
                      (list  "key1"
                             "observation"
                             )
                      #:num-samples 1000
                      #:truncate-output 5
                      ; #:skip-marginals? #t
                      ; #:show-stats? #t
                      ; #:credible-interval 0.84
                      ; #:show-histogram? #t
                      ; #:show-percentiles? #t
                      )

;; Here is a simpler version from http://dicelang.cs.ucla.edu/
(define (model2 reps)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define (sendChar key  observation)
     (define gen (discrete (priors '(0.5 0.25 0.125 0.125)))) ;; sample a FooLang character
     (define enc (+ key gen))                                ;; encrypt the character
     ;; (define enc = (key + gen) % 4                         ;; encrypt the character            
     (observe/fail (= observation enc))
     
     enc
     )

   ;; sample a uniform random key: A=0 B=1 C=2 D=3
   (define key (discrete (priors '(0.25 0.25 0.25 0.25))))
        
   ;; observe the ciphertext CCCC (i.e. 3 3 3 3)
   (repeat (lambda () (sendChar key 3)) reps)
        
   (list key)

   )
  )

(displayln "Model 2:")
(for ([reps (range 1 5)])
  (show "reps" reps)
  (show-marginals (model2 reps)
                  (list  "key"
                         )
                  #:num-samples 1000
                  #:truncate-output 5
                      ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                      ; #:show-percentiles? #t
                  )
  )

