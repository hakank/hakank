#| 

  Number of dice throws until a target is reached in Racket Gamble.

  What is the number of dice throws needed to reach a cumulative
  sum:
   *  larger or equal to a target number.
   *  exactly on target

  Here are two models:

  * model1  
    This model uses set! to update values, which is copied from the PSI model.
    The drawback is that enumerate does not work, but the samplers are
    fairly fast.

    One can see this as a proof of concept (of using set! in Gamble models).

  * model2
    This is a "proper" model using recursion.
   
    Enumerate work quite good for small values, e.g. target=19 takes a couple
    of seconds, but after that it's slow,

    The importance-sampler are fast and is recommended for targets > 19 (or so).

    This models has twp modes:
    - mode "on-target": This means that the sum of dice rolls must be exactly 
      the target
    - mode "not-on-target": This means that the final number can be any number >= target
      (restricted to the constraint that we throw a d6 die).

    Here are some results.

    * n=6 (numerate)

     mode on-target target 6)

     var : s
     6: 1 (1.0)
     mean: 6 (6.0)

     var : n
     mean: 12/7 (1.7142857142857142)

     mode not-on-target target 6)
     var : s
     mean: 117649/15552 (7.564879115226337)

     var : n
     mean: 16807/7776 (2.1613940329218106)

   * n=10 (enumerate)

     mode on-target target 10)
     var : s
     10: 1 (1.0)
     mean: 10 (10.0)

     var : n
     mean: 7671280/2498881 (3.06988608100986)

     mode not-on-target target 10)
     var : s
     mean: 234466225/20155392 (11.632928052205584)

     var : n
     mean: 33495175/10077696 (3.323693729201595)


   * n=21 (enumerate, about 20s for each mode)

     mode on-target target 21)
     var : s
     21: 1 (1.0)
     mean: 21 (21.0)

     var : n
     mean: 39114034072160877/6273265544452129 (6.235035611835697)

     mode not-on-target target 21)
     var : s
     mean: 165736272416903335/7312316880125952 (22.665356976987105)

     var : n
     mean: 23676610345271905/3656158440062976 (6.475816279139173)

  * n=61 (importance-sampler)

    mode on-target target 61)
    var : s
    mean: 61.0

    var : n
    mean: 17.656

    mode not-on-target target 61)
    var : s
    mean: 62.59399999999997

    var : n
    mean: 17.795999999999992

  * n=100 (importance-sampler)

    mode on-target target 100)
    var : s
    mean: 100.0

    var : n
    mean: 28.744000000000003

    mode not-on-target target 100)
    var : s
    mean: 101.67999999999994

    var : n
    mean: 29.02999999999998


  This model is a port of my PSI model number_of_dice_throws_to_target.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


;
; Using set!, but enumerate give strange results (probably as expected).
; Use model2 instead.
;
(define (model1 target)
  (; enumerate ; enumerate give strange results (presuminglydue to set! 
   rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define r (range 1 7))
   ; (define s (uniform-draw r))
   (define s 0)
   (define n 0)
   (for ([i (in-naturals)]
         #:break (>= s target)
         #:do [(set! s (+ s (uniform-draw r)))
               (set! n (add1 n))])
     i)
 
   (observe/fail (= s target))

   (list s n)
 
   )
  )

;; (show-marginals (model1 6)
;;                 (list "s"
;;                       "n"
;;                  )
;;                 #:num-samples 10000
;;                 )

;; (exit)

;
; Model: recursive variant, which works with enumerate 
;
(define (model2 target mode)
  
  (; enumerate ; rather slow for larger targets (> 19 or so)
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define r (range 1 7))
  
   (define (f s n)
     (if (>= s target)
         (list s n)
         (f (+ s (uniform-draw r)) (add1 n))))

   (define res (f 0 0))

   (when (eq? mode "on-target")
     (observe/fail (= (first res) target))
     )

   res
 
   )
  )

(displayln "\nModel 2")
; Note: Change to importance-sampler for value > 19
(for* ([target '(6 10 21 61 100 200 1000)]       
       [mode (list "on-target" "not-on-target")])
  (show2 "\nmode" mode "target" target)
  (time (show-marginals (model2 target mode)
                  (list "s"
                        "n"
                        )
                  #:num-samples 1000
                  #:skip-marginals? #t
                  )
        )
  )
