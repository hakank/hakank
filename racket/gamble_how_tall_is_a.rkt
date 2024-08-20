#| 

  How tall is a in Racket Gamble.

  https://www.allendowney.com/blog/2018/10/18/how-tall-is-a/
  """
  Here are a series of problems I posed in my Bayesian statistics class:

  1) Suppose you meet an adult resident of the U.S. who is 170 cm tall. 
     What is the probability that they are male?

  2) Suppose I choose two U.S. residents at random and A is taller than B.  
     How tall is A?

  3) In a room of 10 randomly chosen U.S. residents, A is the second tallest.  
     How tall is A?  
     And what is the probability that A is male?

  As background: For adult male residents of the US, the mean and standard deviation of 
  height are 178 cm and 7.7 cm. For adult female residents the corresponding stats 
  are 163 cm and 7.3 cm.  And 51% of the adult population is female.
  """

  See below for the individual problems.

  This is a port of my WebPPL model how_tall_is_a.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

#|
  Problem 1:

  1) Suppose you meet an adult resident of the U.S. who is 170 cm tall. 
  What is the probability that they are male?
  
  According to https://www.allendowney.com/blog/2018/10/18/how-tall-is-a/
    female 0.5432225483131837
    male 0.45677745168681627

  This model:

  var : gender
  female: 0.5449101384170736
  male: 0.45508986158292647


|#
(define (how-tall-is-a-1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define gender (categorical-vw2 (vector 0.49 0.51) (vector "male" "female")))
   (define height 
     (if (eq? gender "male")
             (normal-dist 178 7.7)
             (normal-dist 163 7.3)))
   
   (observe-sample height 170)
    
   (list gender)

   )
  )

(displayln "Problem 1;")
(show-marginals (how-tall-is-a-1)
                (list "gender"
                      )
                #:num-samples 10000
                #:truncate-output 1
                ; #:skip-marginals? #t
                )



#|
  Problem 2

  2) Suppose I choose two U.S. residents at random and A is taller than B.  
  How tall is A?

  Solution from https://www.allendowney.com/blog/2018/10/18/how-tall-is-a/
  A: 176.67506663725212
  B: 164.05298129722925

  This model:

  var : height 0
  mean: 176.3816307275412

  var : height 1
  mean: 164.27100310105703

  var : gender 0
  male: 0.7010999999999792
  female: 0.2989000000000234

  var : gender 1
  female: 0.7191999999999772
  male: 0.2808000000000254


|#
(define (how-tall-is-a-2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define gender (mem (lambda (p)
                         (categorical-vw2 (vector 0.49 0.51) (vector "male" "female")))))
   (define height (mem (lambda (p)
                         (if (eq? (gender p) "male")
                             (normal 178 7.7)
                             (normal 163 7.3)))))
   
   (observe/fail (> (height 0) (height 1)))
    
   (list (height 0)
         (height 1)
         (gender 0)
         (gender 1))

   )
  )

(displayln "Problem 2;")
(show-marginals (how-tall-is-a-2)
                (list "height 0"
                      "height 1"
                      "gender 0"
                      "gender 1"                      
                      )
                #:num-samples 10000
                #:truncate-output 1
                ; #:skip-marginals? #t
                )


#|
  Problem 3

  3) In a room of 10 randomly chosen U.S. residents, A is the second tallest.  
  How tall is A?  
  And what is the probability that A is male?
  
    Solution from https://www.allendowney.com/blog/2018/10/18/how-tall-is-a/
    A: 181.60660153115973

    """
    A is 182cm tall, and has a 92.2% chance of being male.
    Also, A has a 80% chance of being between 175.8cm and 188.1cm
    """

  var : height 0
  mean: 186.39764946756642
  Credible interval (0.8): 179.15616021255778..193.33546777664833

  var : height 1
  mean: 181.29608289919872
  Credible interval (0.8): 175.42054360734664..187.3676306265017

  var : gender 0
  male: 0.9660000000000007
  female: 0.034

  var : gender 1
  male: 0.9140000000000007
  female: 0.08600000000000005

|#
(define (how-tall-is-a-3)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; (define gender (mem (lambda (p) 
   ;;                       (categorical-vw2 (vector 0.49 0.51) (vector "male" "female")))))
   (define gender (mem (lambda (p) 
                         (if (flip 0.49) "male" "female"))))
   (define height (mem (lambda (p)
                         (if (eq? (gender p) "male")
                             (normal 178 7.7)
                             (normal 163 7.3)))))
   
   ;; person 0 is taller than all, especially person 1
   (observe/fail (> (height 0) (height 1)))
   ;; and person 1 is taller than anyone else
   (for ([p (range 2 10)])
     (observe/fail (> (height 1) (height p)))
     )
   
   (list (height 0)
         (height 1) ; <---
         ;; (height 2)
         ;; (height 3)
         ;; (height 4)
         ;; (height 5)
         ;; (height 6)
         ;; (height 7)
         ;; (height 8)
         ;; (height 9)        
         
         ;; Gender of the two tallest
         (gender 0)
         (gender 1) ; <--
         
         )

   )
  )

(displayln "Problem 3;")
(show-marginals (how-tall-is-a-3)
                (list "height 0"
                      "height 1" ; <--
                      ;; "height 2"
                      ;; "height 3"
                      ;; "height 4"
                      ;; "height 5"
                      ;; "height 6"
                      ;; "height 7"
                      ;; "height 8"
                      ;; "height 9"

                      "gender 0"
                      "gender 1" ;  <--
                      )
                #:num-samples 1000
                #:truncate-output 1
                ; #:skip-marginals? #t
                #:credible-interval 0.8
                )


;;; (show-freq (repeat (how-tall-is-a-3) 100))

#|

  Just playing with dist-cdf and dist-inv-cdf (problem 2 and 3 w/o observations)

(dist-cdf (height 0) 181): 0.9931637290910468
(dist-cdf (height 0) 173): 0.9146351663914819

dist-inv-cdf
(p 0.05 150.99256852325425)
(p 0.1 153.6446735715244)
(p 0.15 155.43403625669535)
(p 0.2 156.85616499491772)
(p 0.25 158.0762248235686)
(p 0.3 159.1718762572313)
(p 0.35 160.18716059522475)
(p 0.4 161.15056614710866)
(p 0.45 162.08267216795795)
(p 0.5 163.0)
(p 0.55 163.91732783204205)
(p 0.6 164.84943385289134)
(p 0.65 165.81283940477525)
(p 0.7 166.8281237427687)
(p 0.75 167.9237751764314)
(p 0.8 169.14383500508228)
(p 0.85 170.56596374330465)
(p 0.9 172.3553264284756)
(p 0.95 175.00743147674575)
(p 0.99 179.98233948049813)

(p 0.5 163.0)
(p 0.7 166.8281237427687)
(p 0.84 170.2595425474312)
(p 0.9 172.3553264284756)
(p 0.99 179.98233948049813)
(p 0.999 185.55869583502505)
(p 0.9999 190.14882034382669)

mens-height
(p 0.5 178.0)
(p 0.7 182.03788394785192)
(p 0.84 185.6573257007151)
(p 0.9 187.86794705469342)
(p 0.99 195.91287863011448)
(p 0.999 201.79478875749217)
(p 0.9999 206.63642693800895)


|#
(define (test)

  
  (define gender (mem (lambda (p) 
                        (if (flip 0.49) "male" "female"))))  
  (define height (mem (lambda (p)
                           (if (eq? (gender p) "male")
                               (normal-dist 178 7.7)
                               (normal-dist 163 7.3)))))

  (show "(dist-cdf (height 0) 181)" (dist-cdf (height 0) 181))
  (show "(dist-cdf (height 0) 173)" (dist-cdf (height 0) 173))
  
  (newline)
  (displayln "dist-inv-cdf")
 
  (for ([p (range 5 100 5)])
    ; (displayln (list "p" (/ p 100.)))    
    (displayln (list "p" (/ p 100.0) (dist-inv-cdf (height 0) (/ p 100.0))))
    )
  (displayln (list "p" 0.99 (dist-inv-cdf (height 0) 0.99)))
  (newline)
  (for ([p '(0.5 0.7 0.84 0.9 0.99 0.999 0.9999)])
    (displayln (list "p" p (dist-inv-cdf (height 0) p))))
  (newline)
  
  (displayln "mens-height")  
  (define mens-height (mem (lambda (p) (normal-dist 178 7.7))))
  (for ([p '(0.5 0.7 0.84 0.9 0.99 0.999 0.9999)])
    (displayln (list "p" p (dist-inv-cdf (mens-height 0) p))))
  
  )
; (test)
