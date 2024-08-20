#| 

  Smoker social network in Racket Gamble.

  https://dtai.cs.kuleuven.be/problog/tutorial/basic/05_smokers.html
  """
  Social networks (Friends & Smokers)

  The ProbLog program below encodes a variant of the famous Friends & Smokers problem. The 
  first two rules state that there are two possible causes for a person X to smoke, 
  namely X having stress, and X having a friend Y who smokes himself and influences X. 
  Note that, according to our this program, the presence of stress and of (possibly multiple) 
  smoking friends all contribute to the probability of the person X smoking, namely in a 
  noisy-or way (recall the noisy-or in the coin tossing example). Furthermore, the program 
  encodes that if X smokes, (s)he has asthma with probability 0.4.

  The program below considers a scenario with 4 people, having a total of 5 
  friendship relations.
  """

  We observes:
   * (observe/fail (smokes 1))
   * (observe/fail (not (influences 3 1)))


var : smokes 0
#f: 0.5123799999999997
#t: 0.48761999999999994
mean: 0.48761999999999994

var : smokes 1
#t: 0.9999999999999996
mean: 0.9999999999999996

var : smokes 2
#f: 0.5828299999999998
#t: 0.41717
mean: 0.41717

var : smokes 3
#f: 0.5135799999999998
#t: 0.48641999999999974
mean: 0.48641999999999974

var : asthma 0
#f: 0.8061299999999995
#t: 0.19386999999999996
mean: 0.19386999999999996

var : asthma 1
#f: 0.6004299999999995
#t: 0.39957
mean: 0.39957

var : asthma 2
#f: 0.8347699999999996
#t: 0.1652299999999999
mean: 0.1652299999999999

var : asthma 3
#f: 0.8054399999999995
#t: 0.19455999999999996
mean: 0.19455999999999996

var : influences 0 1
#f: 0.8000699999999994
#t: 0.19992999999999994
mean: 0.19992999999999994



  This is a port of my WebPPL model smokers.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (define friends-table '(
                          (0 1)
                          (1 0)
                          (1 3)
                          (2 1)
                          (3 1)))
  
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define people '(0 1 2 3))

   (define stress (mem (lambda (p) 
        (flip 0.3))))

   (define influences (lambda (p1 p2) 
        (if (not (= p1 p2)) (flip 0.2) #f)))

   (define friend (lambda (p1 p2) 
        (member (list p1 p2) friends-table)))
    
    
   (define smokes (mem (lambda (p1)
                     (if (stress p1)
                         #t
                         (if (ormap (lambda (p2)
                                      (and 
                                       (not (= p1 p2))
                                       (friend p1 p2)
                                       (influences p2 p1)
                                       (smokes p2)
                                       )
                                      )
                                    people)
                             #t
                             #f)))))
     
   (define asthma (mem (lambda (p) 
                         (if (smokes p) (flip 0.4) #f))));


   (observe/fail (smokes 1))
   (observe/fail (not (influences 3 1)))
   
   (list (smokes 0)
         (smokes 1)
         (smokes 2)
         (smokes 3)
         
         (asthma 0)
         (asthma 1)
         (asthma 2)
         (asthma 3)


         (stress 0)
         (stress 1)
         (stress 2)
         (stress 3)
         
         (influences 0 1)
         )

   )
  )

(show-marginals (model)
                (list "smokes 0"
                      "smokes 1"
                      "smokes 2"
                      "smokes 3"

                      "asthma 0"
                      "asthma 1"
                      "asthma 2"
                      "asthma 3"

                      "stress 0"
                      "stress 1"
                      "stress 2"
                      "stress 3"
                      

                      "influences 0 1"
                      )
                #:num-samples 100000
                )
