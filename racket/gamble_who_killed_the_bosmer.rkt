#| 

  Who killed the Bosmer problem Racket Gamble.

  From https://swi-prolog.discourse.group/t/similar-einstein-riddle/5142/6
  """
  A Bosmer, was slain. The Altmer claims the Dunmer is guilty. The Dunmer says the Khajiit did it. 
  The Orc swears he didn’t kill the Bosmer. The Khajiit says the Dunmer is lying. If only one of 
  these speaks the truth, who killed the Bosmer?""
  """

  This is a port of my WebPPL model who_killed_the_bosmer.wppl

  Result:
  Khajiit speaks the truth
  Orc is guilty
  ((0 0 0 1 0 0 1 0) : 1 (1.0))


  Cf my CP models:
  * Picat: http://hakank.org/picat/who_killed_the_bosmer.pi
  * SWI-Prolog: http://hakank.org/swi_prolog/who_killed_the_bosmer.pl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

; Print the solution
(define (check people lst desc)
   (for ([p people]
         [x lst])
     (when (= x 1) (displayln (format "~a ~a" p desc))) )
  )

(define (who-killed-the-bosmer)
  (enumerate
   
   (define p 1/2) ; Prior probability

   (define people (list "Altmer" "Dunmer" "Orc" "Khajiit"))
   
   ; Speaks the truth?
   (define AltmerT (bernoulli p))
   (define DunmerT (bernoulli p))
   (define OrcT (bernoulli p))
   (define KhajiitT (bernoulli p))
   
   ; Is guilty?
   (define AltmerG (bernoulli p))
   (define DunmerG (bernoulli p))
   (define OrcG (bernoulli p))
   (define KhajiitG (bernoulli p))
   
   ; A Bosmer, was slain.
   
   ; The Altmer claims the Dunmer is guilty.
   (observe/fail (= AltmerT DunmerG))
   
   ; The Dunmer says the Khajiit did it.
   (observe/fail (= DunmerT KhajiitG))
   
   ; The Orc swears he didn’t kill the Bosmer.
   (observe/fail (eq? (= OrcG 0) (= OrcT 1)))
   
   ; The Khajiit says the Dunmer is lying.
   (observe/fail (eq? (= DunmerT 0) (= KhajiitT 1)))
   
   ; If only one of these speaks the truth, who killed the Bosmer?""
   (observe/fail (= (+ AltmerT DunmerT OrcT KhajiitT) 1)) 
   (observe/fail (= (+ AltmerG DunmerG OrcG KhajiitG) 1))

   ; Show the result
   (check people (list AltmerT DunmerT OrcT KhajiitT) "speaks the truth")   
   (check people (list AltmerG DunmerT OrcG KhajiitG) "is guilty")   
   
   (list AltmerT DunmerT OrcT KhajiitT AltmerG DunmerG OrcG KhajiitG)

   )
  )

(show-model (who-killed-the-bosmer))
