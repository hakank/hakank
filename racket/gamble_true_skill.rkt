#| 

  (Simple) True skill Racket Gamble.

  Example inspired by
  Johannes Borgstrom, Andrew D. Gordon, Michael Greenberg, James Margetson, and Jurgen Van Gael:
  "Measure Transformer Semantics for Bayesian Machine Learning"
  https://www.microsoft.com/en-us/research/publication/measure-transformer-semantics-for-bayesian-machine-learning-2011/?from=http%3A%2F%2Fresearch.microsoft.com%2Fpubs%2F135344%2Fmsr-tr-2011-18.pdf

  In this setup the three persons a, b and c has some skill, and they have a
  performance (of some unidentified task/action). We only observe performance, 
  but not knowing the skills.
  Here we observe that:
    - a performs better than both b and c
    - b berforms better than c.

var : skill a
mean: 101.66644833512825

var : skill b
mean: 99.97599618686067

var : skill c
mean: 98.23244976630114

var : performance a
mean: 104.38642065631676

var : performance b
mean: 100.02227023708548

var : performance c
mean: 95.69449036448582

var : performance a > performance b
mean: 1.0000000000000007

var : performance a > performance c
mean: 1.0000000000000007

var : performance b > performance c
mean: 1.0000000000000007

var : skill a > skill b
mean: 0.6610000000000005

var : skill a > skill c
mean: 0.7860000000000006

var : skill b > skill c
mean: 0.6750000000000005


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (true-skill) 
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ;; There are three people, a, b, and c
   
   ;; Each person has an unknown Skill and a
   ;; known performance, where the skill is
   ;; reflected in the performance (with uncertainties).
   
   
   (define skill (mem (lambda (p) 
                        (normal 100 (sqrt 10)))))
   
   (define performance (mem (lambda (p) 
                              (normal (skill p) (sqrt 15)))))
   
   ;; Now we see that a is better than b and c, and b is better than c.
   (observe/fail (> (performance "a") (performance "b")))
   (observe/fail (> (performance "a") (performance "c")))
   (observe/fail (> (performance "b") (performance "c")))        
   
   ;; What are their performance and their (underlying) skills?    
   (list (skill "a")
         (skill "b")
         (skill "c")
         (performance "a")
         (performance "b")
         (performance "c")     
         (> (performance "a") (performance "b"))
         (> (performance "a") (performance "c"))
         (> (performance "b") (performance "c"))          
         (> (skill "a") (skill "b"))
         (> (skill "a") (skill "c"))
         (> (skill "b") (skill "c"))          
         )
   
   )
  )

(show-marginals (true-skill)
                (list "skill a"
                      "skill b"
                      "skill c"
                      "performance a"
                      "performance b"
                      "performance c"
                      "performance a > performance b"
                      "performance a > performance c"
                      "performance b > performance c"                      
                      "skill a > skill b"
                      "skill a > skill c"
                      "skill b > skill c"                      
                      )
                #:num-samples 1000
                #:skip-marginals? #t
                )


  
