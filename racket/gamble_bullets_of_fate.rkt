#| 

  Bullets of fate in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/1008
  """
  In a room stand N armed and angry people. At each chime of a clock, everyone 
  simultaneously spins around and shoots a random other person. The persons who 
  get shot, fall dead and the survivors spin and shoot again at the next chime. 
  Eventually, either everyone is dead or there is a single survivor.

  As N grows, what is the limiting probability that there will be a survivor?

  Warning: I could not solve it. The solution only shows what I tried.
  """

  The limiting probability is around 0.5.

  Cf
  https://colab.research.google.com/gist/varun-seth/c57ed32726bf7492daa9dea375398c09/puzzle_1008_rdd_exact.ipynb

  which gives the same exact probabilities using enumerate:

  n: 3
  var : p
  #t: 3/4 (0.75)
  #f: 1/4 (0.25)
  mean: 3/4 (0.75)

  n: 4
  var : p
  #t: 16/27 (0.5925925925925926)
  #f: 11/27 (0.4074074074074074)
  mean: 16/27 (0.5925925925925926)

  n: 5
  var : p
  #f: 17/32 (0.53125)
  #t: 15/32 (0.46875)
  mean: 15/32 (0.46875)

  n: 6
  var : p
  #f: 16421/28125 (0.5838577777777778)
  #t: 11704/28125 (0.41614222222222225)
  mean: 11704/28125 (0.41614222222222225)

  n: 7
  var : p
  #f: 1413739/2519424 (0.5611357992938069)
  #t: 1105685/2519424 (0.43886420070619314)
  mean: 1105685/2519424 (0.43886420070619314)



  * Using enumerate for larger values

  n: 3
  mean: 0.7523

  n: 4
  mean: 0.5877

  n: 5
  mean: 0.4797

  n: 6
  mean: 0.4169

  n: 7
  mean: 0.4367

  n: 10
  mean: 0.5439

  n: 50
  mean: 0.4767

  n: 100
  mean: 0.5138


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(require racket/set)

(define (model n)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define people (shuffle (range n)))
   ; (define people (draw-without-replacement n (range n)))
   (define people (shuffle (range n)))
  
   (define (f a)
     (if (<= (length a) 1)
         a
         (let ([targets (remove-duplicates (for/list([p a]) (uniform-draw (remove p a))))])
           (f (set-symmetric-difference a targets)))))
   
   (define a (f people))
   (define len (length a))
   (define survivor (if (> (length a) 0) (first a) a))
   (define p (not (eq? a '())))

   (list p
         )
   
   )
)

(for ([n '(3 4 5 6 7 10 50 100)])
      (show "n" n)
      (show-marginals (model n)
                      (list "p"
                       )
                      #:num-samples 10000
                      ; #:truncate-output 5
                      ; #:skip-marginals? #t
                      )
  )
  

