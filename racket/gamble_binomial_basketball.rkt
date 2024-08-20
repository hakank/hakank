#| 

  Binomial basketball in Racket Gamble.

  From https://reference.wolfram.com/language/ref/BinomialDistribution.html
  """
  A basketball player has a free-throw percentage of 0.75. 

  Find the probability that the player hits 2 out of 3 free throws in a game.
  [free_throwIs2]
    Answer: 0.421875

  Find the probability that the player hits the last 2 of 5 free throws. 
  [q1]
     Answer: 0.00878906
   

  Find the expected number of hits in a game with n free throws. 
  [q2]
    Answer: 0.75 n
  """

  Find the probability that the player hits 2 out of 3 free throws in a game.
  (#f : 0.578125)
  (#t : 0.42187499999999994)

  Find the probability that the player hits the last 2 of 5 free throws.
  (#f : 0.9912109375000001)
  (#t : 0.008789062500000007)

  Find the expected number of hits in a game with n free throws.  
  7.49748

  Combine the questions 
  ((#f #f) : 0.5730438232421875)
  ((#t #f) : 0.41816711425781244)
  ((#f #t) : 0.0050811767578125035)
  ((#t #t) : 0.0037078857421875048)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

; -> (discrete-dist [#f 0.578125] [#t 0.42187499999999994])
(displayln "Find the probability that the player hits 2 out of 3 free throws in a game.")
(show-discrete-dist
 (enumerate
  (define p 0.75)
  (define free-throw (binomial 3 p))
  (define free-throwIs2 (= free-throw 2))
  free-throwIs2
  )
 )
(newline)

; -> (discrete-dist [#f 0.9912109375000001] [#t 0.008789062500000007])
(displayln "Find the probability that the player hits the last 2 of 5 free throws.")
(show-model (enumerate
                     (define p 0.75)
                     (define free-throw2-a (binomial 3 p))
                     (define free-throw2-b (binomial 2 p))
                     (and (= free-throw2-a 0) (= free-throw2-b 2))
                     )
                    )

(newline)

; > 7.5
(displayln "Find the expected number of hits in a game with n free throws.  ")
(define test3
  (enumerate
   
   (define p 0.75)
   (define free_throw3 (binomial 10 p))
   
   free_throw3
   )
  )
; (exact->inexact (sampler->mean test3 100000))
(show-model test3)
(newline)

;;; How do I separate p1 and p2, i.e. just their individual marginals
; -> (discrete-dist ['(#f #f) 0.5730438232421875] ['(#f #t) 0.0050811767578125035] ['(#t #f) 0.41816711425781244] ['(#t #t) 0.0037078857421875048])
(displayln "Combine the questions ")
(show-model
 (enumerate
  (define p 0.75)
  
  ; q1
  (define free-throw1 (binomial 3 p))
  (define p1 (= free-throw1 2))
  
  ; q2
  (define free-throw2-1 (binomial 3 p))
  (define free-throw2-2 (binomial 2 p)) 
  (define p2 (and (= free-throw2-1 0) (= free-throw2-2 2)))
  
  (list p1 p2)
  )
 )
(newline)

; Later: (show-marginals) is one way of showing all the margins in
;        one run.
(define (combined)

  (enumerate
   (define p 0.75)
   
   ; q1
   (define free-throw1 (binomial 3 p))
   (define p1 (= free-throw1 2))
  
   ; q2
   (define free-throw2-1 (binomial 3 p))
   (define free-throw2-2 (binomial 2 p)) 
   (define p2 (and (= free-throw2-1 0) (= free-throw2-2 2)))
   
   (list p1 p2)
   
   )
  )

(displayln "\nShow the marginals:")
(show-marginals (combined) '("p1" "p2"))


;; (define test4
;;   (rejection-sampler
;;    ; importance-sampler ; seems to be slower
;;    ; mh-sampler
;;    (define p 0.75)
;;    (define n 2)
;;    (define free-throw (binomial 3 p))
;;    (define p1 (= free-throw 2))
;;    (define free-throw2-a (binomial 3 p))
;;    (define free-throw2-b (binomial 2 p))
;;    (define p2 (and (= free-throw2-a 0) (= free-throw2-b 2)))

;;    ; (define free_throw3 (binomial 10 p))
;;    ; (observe/fail (and (eq? p1 #t) (eq? p2 #t)))
;;    ;;; (observe/fail (and (eq? p1 #t) (eq? p2 #t)))   
;;    ; (list p1 p2)
;;    ; (and (eq? p1 #t) (eq? p2 #t))
;;    p1
;;    )
;;   )

;; (sampler->discrete-dist test4 10000)
