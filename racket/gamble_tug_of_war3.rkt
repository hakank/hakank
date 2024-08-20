#| 

  Tug of war in Racket Gamble.

  This is a port of the Church ForestDB model.

  The Church version (as well as the Gamble port) shows a list of values, i.e.

  ((10 10) : 0.40239043824701193)
  ((5 10) : 0.27191235059760954)
  ((10 5) : 0.23804780876494025)
  ((5 5) : 0.08764940239043825)

  Though Church can show a histogram version of this.

  I don't know how to get each person's separate probability distribution (marginals),
  such as in WebPPL. It seems that Church doesn't have this feature as well; Church just
  returns a list of list.
   
  One approach is to just run for a single measure, here (strength person):

  * sample-2 

alice
(10 : 0.6526946107784432)
(5 : 0.3473053892215569)
(mean: 8.264471057884231)

bob
(10 : 0.6387225548902196)
(5 : 0.36127744510978044)
(mean: 8.194610778443113)

sue
(5 : 0.6497005988023952)
(10 : 0.3502994011976048)
(mean: 6.75249500998004)

tom
(5 : 0.6097804391217565)
(10 : 0.3902195608782435)
(mean: 6.952095808383233)

  One drawback of this approach is that it require many more calls than in WebPPL (etc).


  I also tried with enumerate, but that's too slow, probably due to the many 
  observe/fail entries.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;;;
;;; """
;;; Tug of War
;;; The “tug of war” model is a hierarchical model that can be used to illustrate the
;;; broad range of inferences that can be drawn from a relatively simple compositional
;;; model.
;;; """
;;;
(define num-people 4)

(define alice 0)
(define bob 1)
(define sue 2)
(define tom 3)

(define team1 (list alice bob))
(define team2 (list sue tom))

(define (sample-strength)
  (if (flip) 10 5))

(define (sample person)
  (rejection-sampler
   ; importance-sampler ; error
   ; mh-sampler ; error
   
   (define strengths
     (repeat sample-strength num-people))
   
   (define (strength person)
     (list-ref strengths person))
   
   (define lazy (lambda (person) (flip 1/3)))
   
   (define (total-pulling team)
     (sum
      (map (lambda (person)
             (if (lazy person) (/ (strength person) 2) (strength person)))
           team)))
   
   (define (winner team1 team2)
     (if (< (total-pulling team1)
            (total-pulling team2))
         'team2
         'team1))
    
   (observe/fail (and (eq? 'team1 (winner team1 team2))
                      (eq? 'team2 (winner team1 team2))
                      (eq? 'team1 (winner team1 team2))
                      (eq? 'team1 (winner team1 team2))
                      (eq? 'team1 (winner team1 team2))
                      (eq? 'team1 (winner team1 team2))
                      (eq? 'team1 (winner team1 team2))
                      (eq? 'team1 (winner team1 team2))))

   (strength person)
   )
  )


(define people '(alice bob sue tom))
(for ([id (list alice bob sue tom)])
  (newline)
  (displayln (list-ref people id))
  (show-freq (repeat (sample id) 1000))
  )

