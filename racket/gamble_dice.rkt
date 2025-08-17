#| 

  Dice problem in Racket Gamble.

  From gamble example dice.rkt
  """
  Somebody gives Norman a hat containing five slips of paper, numbered 1
  to 5 respectively. Norman draws a slip from the hat. The number on the
  slip is called n. Norman then repeats the following procedure ten
  times:

  Take n dice from the bag, throw them, report the total t, then put the
  dice back in the bag.

  The totals reported are 21, 15, 34, 12, 18, 38, 46, 13, 24, and 27.

  The question is, what is the number on the slip Norman drew? (That is,
  what is n?)

  Assume all of the dice in the bag are 20-sided.
  """

  Running the example/dice.rkt program (s-smart model) gives:
  #<discrete-dist: [3 0.9999025922172229] [4 9.740770226150044e-5] [5 8.051562423150861e-11]>


  This program:

  Using sorted list of *totals* and (observe-sample (normal-dist ...)) gives
  a fairly good result:
  variable : n
  3: 0.9999999923997364
  2: 7.600263633271818e-9
  4: 2.1239114928788854e-100
  1: 3.8580050369840855e-292
  mean: 2.999999992399736

  However, using the original *totals* order gives the wrong answer:
  variable : n
  2: 0.9999998144608981
  3: 1.8553910180548103e-7
  1: 0.0
  mean: 2.000000185539102


  However, with a little trickery we can use observe-sample with binomial-dist
  which is fairly fast (5.7s):
  variable : n
  3: 0.9999999999998866
  4: 1.1338856754308133e-13
  5: 1.5213770541720718e-33
  mean: 3.0000000000001132


  For 100000 samples (38s):
  3: 0.9999365920526166
  4: 6.340794738337384e-5
  5: 8.187980010948882e-34
  mean: 3.0000634079473834


  The 'trickery' is to find the smallest possible n by notice that 46 implies 
  that n must be at least 46/20 = 2.3, i.e. at least n = 3 dice.
  The reason we have to do this is that binomial-dist croaks if the 
  probability parameter is > 1 which happens when n < 3.

  Unfortunately, enumerate is too slow for this.

  Cf gamble_how_many_times_did_i_flip_the_coin.rkt

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

(define *totals* '(21 15 34 12 18 38 46 13 24 27))
; (define *totals* (sort '(21 15 34 12 18 38 46 13 24 27) <))

(define totals-len (length *totals*))

(define (model)
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler


   ; Figure out the minimum value of n based on the values seen
   (define min-n (ceiling (/ (max-list *totals*) 20)))
   
   ; (define n (add1 (discrete-uniform 5)))
   (define n (+ min-n (discrete-uniform (add1 (- 5 min-n)))))
  
   ; No sort
   (define s (for/list ([t totals-len]) (for/sum ([i n]) (add1 (discrete-uniform 20)))))

   ;; Using Sorted totals
   ;; (define s (sort (for/list ([t totals-len])
   ;;                   (for/sum ([i n]) (add1 (discrete-uniform 20)))) <))

  
   ; (show2 "n:" n "s:" s)
   (for ([i (range totals-len)])
     ; This is extremly slow:
     ; (observe/fail (eq? (list-ref *totals* i) (list-ref s i)))

     ; This works for sorted *total*, but not for unsorted total:
     ; (observe-sample (normal-dist (list-ref *totals* i) 1) (list-ref s i))

     ; Using (binomial-dist n p) seems to work
     ;   * We throw n dice, i.e. get a point of n*20
     ;   * The probability p is then. <observed value in total> / n * 20
     ; However, enumerate seems to be very slow, so we use importance-sampler
     (define p (/ (list-ref *totals* i) (* n 20.0)))
     (observe-sample (binomial-dist (* n 20) p) (list-ref s i))
     )

   ; (show2 n)
   
   (list n)
   )
  )

(show-marginals (model)
                (list "n"
                      )
                #:num-samples 10000
                #:truncate-output 5
                )


