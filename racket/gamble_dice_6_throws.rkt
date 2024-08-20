#| 

  Dice 6 throws problem in Racket Gamble.

  http://cplint.eu/example/inference/dice.swinb

  """
  A six-sided die is repeatedly thrown until the outcome is six. on(T,F) 
  means that on the Tth throw the face F came out.
  """

  What is the probability that the die lands on face 1 at time 0?


  Output:
  dice-6-throws t: 0 v: 1 obs-list: (((0 1) #t))
  (#f : 5/6 (0.8333333333333334))
  (#t : 1/6 (0.16666666666666666))

  dice-6-throws t: 1 v: 1 obs-list: (((0 1) #t))
  (#f : 31/36 (0.8611111111111112))
  (#t : 5/36 (0.1388888888888889))

  dice-6-throws t: 2 v: 1 obs-list: (((0 1) #t))
  (#f : 191/216 (0.8842592592592593))
  (#t : 25/216 (0.11574074074074074))

  dice-6-throws t: 2 v: 1 obs-list: (((0 1) #t) ((1 1) #t) ((2 1) #t))
  (#f : 191/216 (0.8842592592592593))
  (#t : 25/216 (0.11574074074074074))

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (dice-6-throws t-val v-val obs-list)
  (displayln (format "dice-6-throws t: ~a v: ~a obs-list: ~a" t-val v-val obs-list))
  
  (enumerate

   ;;  Throw a single dice
   (define (throw-dice t) (categorical-vw (vector 1 2 3 4 5 6) (vector 1/6 1/6 1/6 1/6 1/6 1/6)))
   
   (define (on t v)
     ; (writeln (format "on ~a ~a" t v))
     (let ([throw-dice-t (throw-dice t)])
       ; (displayln (list "throw-dice-t" throw-dice-t))
       (if (= t 0)
           (if (and (= throw-dice-t v) (not (= v 6))) #t #f)
           (if (and (not (= throw-dice-t 6)) ; This is not a 6
                    (= throw-dice-t v)       ; This is v
                    (= 0 (for/sum ([p (range t)]) ; No previous 6's
                           (if (= (throw-dice p) 6) 1 0)
                           )))
               #t
               #f))
       ))

   ; (observe/fail (eq? (on 0 1) #t))
   ; (observe/fail (eq? (on 0 1) #t))   
   ; (observe/fail (eq? (on 1 1) #t))   
   (for ([obs obs-list])
     (case obs
       [(t v tt) (observe/fail (eq? (on 0 1) #t))]))

   (on t-val v-val)
   
   )
  )

(let ([obs (list '((0 1) #t) )])
  (show-model (dice-6-throws 0 1 obs ))
  (newline)
  (show-model (dice-6-throws 1 1 obs))
  (newline)
  (show-model (dice-6-throws 2 1 obs))
)
(newline)
(show-model (dice-6-throws 2 1 (list '((0 1) #t)
                                             '((1 1) #t)
                                             '((2 1) #t))))
                                             
