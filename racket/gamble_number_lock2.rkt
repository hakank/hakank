#| 

  Number lock in Racket Gamble.

  From Presh Talwalkar (MindYourDecisions) 
  """
  Puzzles like this have been shared with the dubious claim that "only a
  genius can solve" them. But they are still fun problems so let's work one
  out.

  A number lock requires a 3 digit code. Based on these hints, can you crack
  the code?

   682 - one number is correct and in the correct position
   645 - one number is correct but in the wrong position
   206 - two numbers are correct but in the wrong positions
   738 - nothing is correct
   780 - one number is correct but in the wrong position

  Video:  https://youtu.be/-etLb-8sHBc
  """

  This model use the same approach as
  - MiniZinc model: http://hakank.org/minizinc/number_lock.mzn
  - Picat model: http://hakank.org/picat/number_lock.pi

  Result: 
    var : x
    (0 5 2): 1 (1.0)

  This model is a port of my WebPPL model number_lock2.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 3) ;; number of columns (i.e. length of the numbers)
   (define m 5); ;; number of rows (i.e. number of hints)
   
   ; The numbers
   (define y '((6 8 2)
               (6 4 5) 
               (2 0 6) 
               (7 3 8) 
               (7 8 0)
               ))
   
   (define hints '((1 1) 
                   (0 1) 
                   (0 2) 
                   (0 0) 
                   (0 1)))
   
   ;; The unknown number
   (define x (mem (lambda(i) (random-integer 10))))
   
   ;; Number of digits that has correct position and value.
   (define num_correct_positions (mem (lambda (r)
                                        (for/sum ([i n])
                                          (if (= (list-ref2d y r i) (x i)) 1 0)))))
   
   
   ;; Number of digits that has correct value (wether in correct position or not)
   (define num_correct_values (mem (lambda (r)
                                     (for*/sum ([i n]
                                                [j n])
                                       (if (eq? (list-ref2d y r j) (x i)) 1 0)))))

   ; Total number of rows that satisfies the constraints
   (define total (for/sum ([r m])
                   (if (and (= (num_correct_positions r) (list-ref2d hints r 0))
                            (= (num_correct_values r) (list-ref2d hints r 1)))
                       1 0)
                     ))

   (observe/fail (= m total))
     
   (list (for/list ([i n]) (x i)))
   
   )
  )

(show-marginals (model)
                (list "x"
                 )
                
                )
