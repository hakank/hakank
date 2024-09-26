#| 

  Birthday coincidence in Racket.Gamble 

  From https://www.doc.ic.ac.uk/~mpd37/teaching/2014/ml_tutorials/2014-10-15-wood-anglican.pdf
  page 18f:
  """
  Birthday Coincidence
  Approximately, what’s the probability that in a room
  filled with 23 people at least one pair of people
  have the same birthday? 

  (assume birthday (mem (lambda (i) (uniform-discrete 1 366))))
  (assume N 23)
  (assume pair-equal
    (lambda (i j)
      (if (> i N)
        false
          (if (> j N)
            (pair-equal (+ i 1) (+ i 2))
            (if (= (birthday i) (birthday j))
              true
              (pair-equal i (+ j 1)))))))
  (predict (pair-equal 1 2)
  """

  0: 0
  1: 0
  2: 0
  3: 0
  4: 0.02
  5: 0.02
  6: 0.08
  7: 0.04
  8: 0.09
  9: 0.12
  10: 0.06
  11: 0.16
  12: 0.16
  13: 0.23
  14: 0.32
  15: 0.25
  16: 0.31
  17: 0.18
  18: 0.35
  19: 0.39
  20: 0.37
  21: 0.43
  22: 0.43
  23: 0.48
  24: 0.52
  25: 0.58
  26: 0.59
  27: 0.7
  28: 0.67
  29: 0.66
  30: 0.72


  This is a port of my WebPPL model birthday_coincidence.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model n)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (defmem (birthday i) (add1  (random-integer 365)))

   (define (pair_equal i j)
      (if (> i n)
          #f
          (if (> j n) 
              (pair_equal (add1 i) (+ 2 i))
              (if (= (birthday i) (birthday j))
                  #t
                  (pair_equal i (add1 j))))))
   
   (define p (pair_equal 1 2))
   
   p
   )
)

(for ([n 31])
  (displayln (format "~a: ~a" n  (dist-pdf (sampler->discrete-dist (model n) 100) #t)))
)
