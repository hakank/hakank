#| 

  Family Out problem Racket Gamble.

   From Eugene Charniak
   "Bayesian Networks without Tears"
   page 51
   
   """
   Suppose when I go home at night, I want to know if my family is home before I try the doors. 
   (Perhaps the most convenient door to enter is double locked when nobody is home.) 
   Now, often when my wife leaves the house, she turns on an outdoor light. However, 
   she sometimes turns on this light if she is expecting a guest. Also, we have a dog. 
   When nobody is home, the dog is put in the back yard. The same is true if the dog
   has bowel troubles. Finally, if the dog is in the backyard, I will probably hear her barking (or
   what I think is her barking), but sometimes I can be confused by other dogs barking. This
   example, partially inspired by Pearl’s (1988) earthquake example, is illustrated in figure 1.
   There we find a graph not unlike many we see in AI. We might want to use such diagrams to
   predict what will happen (if my family goes out, the dog goes out) or to infer causes from
   observed effects (if the light is on and the dog is out, then my family is probably out).

   The important thing to note about this example is that the causal connections are
   not absolute. Often, my family will have left without putting out the dog or turning on a
   light. Sometimes we can use these diagrams anyway, but in such cases, it is hard to know
   what to infer when not all the evidence points the same way. Should I assume the family is
   out if the light is on, but I do not hear the dog? What if I hear the dog, but the light is
   out? Naturally, if we knew the relevant probabilities, such as P(family-out | light-on, ¬ hear-
   bark), then we would be all set. However, typically, such numbers are not available for
   all possible combinations of circumstances. Bayesian networks allow us to calculate them
   from a small set of probabilities, relating only neighboring nodes.
   """

var : family out
#t: 0.5005517275895841
#f: 0.49944827241041606
mean: 0.5005517275895841

var : bowel problem
#f: 0.9937720740575253
#t: 0.006227925942474713
mean: 0.006227925942474713

var : light on
#t: 1.0
mean: 1.0

var : dog out
#f: 0.5737198079886079
#t: 0.4262801920113922
mean: 0.4262801920113922

var : hear bark
#f: 1.0
mean: 0 (0.0)

  This is a port of my WebPPL model family_out_problem.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (family-out-problem)

  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   
   (define family-out (flip 0.15))
   (define bowel-problem (flip 0.01))
   (define light-on
     (if family-out
         (flip 0.6)
         (flip 0.05)))
   (define dog-out
     (cond
       [(and family-out bowel-problem)             (flip 0.99)]
       [(and family-out (not bowel-problem))       (flip 0.90)]
       [(and (not family-out) bowel-problem)       (flip 0.97)]
       [(and (not family-out) (not bowel-problem)) (flip 0.30)]
       [else #f]))
    
   (define hear-bark (if dog-out (flip 0.7) (flip 0.01)))
    
    #|
      """
      To take the earlier example, if I observe that the light is on (light-on = true) but 
      do not hear my dog (hear-bark = false), I can calculate the (observe/fail al probability 
      of family-out given these pieces of evidence. (For this case, it is .5.)
      """
   |#
   
    (observe/fail light-on)
    (observe/fail (not hear-bark))

    (list family-out
          bowel-problem
          light-on
          dog-out
          hear-bark)
    
    )
  )

(show-marginals (family-out-problem)
                  (list "family out"
                        "bowel problem"
                        "light on"
                        "dog out"
                        "hear bark"
                        )
                  #:num-samples 1000
                  #:truncate-output 5
                  ; #:skip-marginals? #t
                  )


