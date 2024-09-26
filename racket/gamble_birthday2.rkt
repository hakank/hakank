#| 

  Birthday in Racket.Gamble 

  From PSI test/birthday3.psi

  For d=7,p=3 (method:enumerate)
  duplicate
  Marginal:
    false : 0.6122448979591837
    true : 0.3877551020408163

  This model:
  var : birthday
  (0 1 0 1 0 0 1): 6/343 (0.01749271137026239)
  (0 0 1 1 0 0 1): 6/343 (0.01749271137026239)
  (1 1 0 1 0 0 0): 6/343 (0.01749271137026239)
  (0 1 0 0 1 0 1): 6/343 (0.01749271137026239)
  (1 0 0 0 1 1 0): 6/343 (0.01749271137026239)
  ...
  (0 0 3 0 0 0 0): 1/343 (0.0029154518950437317)
  (0 0 0 3 0 0 0): 1/343 (0.0029154518950437317)
  (0 0 0 0 0 0 3): 1/343 (0.0029154518950437317)
  (3 0 0 0 0 0 0): 1/343 (0.0029154518950437317)
  (0 0 0 0 0 3 0): 1/343 (0.0029154518950437317)

  var : duplicate
  0: 30/49 (0.6122448979591837)
  1: 19/49 (0.3877551020408163)
  mean: 19/49 (0.3877551020408163)


  This is a port of my WebPPL model birthday2.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   
   (define d 7)
   (define p 3)

   (defmem (birthday b) (random-integer d))

   (define birthdays (for/list ([i d])
                       (for/sum ([j p]) (boolean->integer (= i (birthday j))))))
   (list birthdays
         (for/sum ([b birthdays]) (boolean->integer (> b 0)))
    )

   )
)

(show-marginals (model)
                (list  "birthday"
                       "duplicates"
                       ))
