#| 

  Sally Rooney Hat Puzzle in Racket/Rosette.

  https://www.theguardian.com/science/2024/dec/09/can-you-solve-it-that-sally-rooney-hat-puzzle  
  """
  Can you solve it? That Sally Rooney hat puzzle

  Midway though the new Sally Rooney novel, Intermezzo, two of the characters 
  discuss a puzzle about hats. I thought it would make a perfect puzzle for 
  this column, so here it is.

  A liar who always lies says "All my hats are green."

  Can we conclude that he has some hats?

  In fact, I would like to extend the puzzle by making it multiple choice. 
  Which, if any, of the following statements can we conclude from what the liar has said?

  A) The liar has at least one hat.
  B) The liar has only one green hat.
  C) The liar has no hats.
  D) The liar has at least one green hat.
  E) The liar has no green hats.

  Note: this question was originally set in a maths exam, so the answer assumes 
  some basic assumptions about formal logic. A liar is someone who only says false statements.
  """

  All solutions for 2 hats:
  ((1 0 0 0 1) (1 0 0 0 1) (1 1 0 1 0) (1 0 0 0 1) (1 0 0 0 1) (1 1 0 1 0) (1 0 0 0 1))

  Note that the first element (interpretation A) is 1 in all solutions, which mean
  that the interpretation 
     A) The liar has at least one hat.
  is the correct one.

  For the other interpretations (B..E), there are all 0s and 1s (i.e. not true in 
  all solutions).

  Here are the the number of solutions and the true interpretation for 2..5 hats
  (n 2)
  (num-solutions 7)
  Correct statement(s):
  (A)

  (n 3)
  (num-solutions 37)
  Correct statement(s):
  (A)

  (n 4)
  (num-solutions 175)
  Correct statement(s):
  (A)

  (n 5)
  (num-solutions 781)
  Correct statement(s):
  (A)


  Cf my Picat model: http://hakank.org/picat/sally_rooney_hat_puzzle.pi

  HT to @iamreddave (Twitter/X)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(require "rosette_utils.rkt")

(require rosette/solver/smt/z3)
(current-solver (z3 #:logic 'QF_FD))

(define (hat-puzzle [n 2])
  (clear-vc!)
  
  ; n: Number of hats in the Universe  
  (show "n" n)

  ; Is this hat owned by the speaker?
  (define hats (for/list ([i n])
                 (define-symbolic* hats integer?)
                 (assert (<= 0 hats 1))
                 hats))

  ; Is this hat green?
  (define green-hats (for/list ([i n])
                       (define-symbolic* green-hats integer?)
                       (assert (<= 0 green-hats 1))
                       green-hats))

  (define-symbolic* num-hats integer?)
  (assert (<= 0 num-hats n))

  (define-symbolic* num-green-hats integer?)
  (assert (<= 0 num-green-hats n))

  ; The truth value of the statement of the speaker
  (define-symbolic* statement integer?)
  (assert (<= 0 statement 1))

  ; The different interpretations
  (define-symbolic* A integer?)
  (assert (<= 0 A 1))
  
  (define-symbolic* B integer?)
  (assert (<= 0 B 1))
  
  (define-symbolic* C integer?)
  (assert (<= 0 C 1))
  
  (define-symbolic* D integer?)
  (assert (<= 0 D 1))
  
  (define-symbolic* E integer?)
  (assert (<= 0 E 1))
  
  ; Number of hats owned by the speaker 
  (assert (= num-hats (sum hats)))

  ; Number of green hats own by the speaker
  (assert (= num-green-hats (sum (map (lambda (h g) (b2i (and (= h 1) (= g 1) ))) hats green-hats))))

  ; The speaker says: All my hats are green 
  ; i.e. the number of my hats = the number of my green hats
  (assert (<=> (= statement 1) (= num-hats num-green-hats)))

  ; ... but that's a lie
  (assert (= statement 0))

  ; The different interpretations to for truthfulness
  
  ; A) The liar has at least one hat.
  (assert (<=> (= A 1) (>= num-hats 1)))
  
  ; B) The liar has only one green hat.
  (assert (<=> (= B 1) (= num-green-hats 1)))
  
  ; C) The liar has no hats.
  (assert (<=> (= C 1) (= num-hats 0)))
  
  ; D) The liar has at least one green hat.
  (assert (<=> (= D 1) (>= num-green-hats 1)))
  
  ; E) The liar has no green hats.
  (assert (<=> (= E 1) (= num-green-hats 0)))

  (get-all-solutions (list A B C D E))
  )


(displayln "All solutions for 2 hats:")
(displayln (hat-puzzle 2))
(newline)
(for ([n (range 2 6)])
  (define sols (hat-puzzle n))
  
  (define h (make-hash))
  (for ([sol sols])
    (let-values ([(a b c d e )  (apply values sol)])
      (for ([s sol]
            [c "ABCDE"])
        (when (= s 1) (hash-set! h c (add1 (hash-ref h c 0))))
        )))
  
  ; Check which statement(s) are true in all solutions
  (define num-solutions (length sols))
  (show "num-solutions" num-solutions)
  (displayln "Correct statement(s):")
  (displayln (for/list ([k (hash-keys h)]
                        #:when (= (hash-ref h k) num-solutions))
               (string k)))
  (newline)
  )
