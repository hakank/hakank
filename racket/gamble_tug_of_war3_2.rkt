#| 

  Tug of war in Racket Gamble.

  This is a port of the Church ForestDB model.

  The Church version (as well as the Gamble port) shows a list of values, i.e.

  ((10 10) : 0.40239043824701193)
  ((5 10) : 0.27191235059760954)
  ((10 5) : 0.23804780876494025)
  ((5 5) : 0.08764940239043825)

  Though Church can show a histogram version of this.

  This is a variant of gamble_tug_of_war3.rkt which uses (show-marginals) which
  needs only a single run of the solver.

var : strength alice
10: 0.6469999999999999
5: 0.3529999999999999
mean: 8.235

var : strength bob
10: 0.6519999999999998
5: 0.3479999999999999
mean: 8.259999999999998

var : strength sue
5: 0.6339999999999999
10: 0.3659999999999999
mean: 6.829999999999998

var : strength tome
5: 0.6189999999999999
10: 0.3809999999999999
mean: 6.904999999999998


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

(define (sample)
  
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

   (list
    (strength alice)
    (strength bob)
    (strength sue)
    (strength tom))
   
   )
  )


;; (define people '(alice bob sue tom))
;; (for ([id (list alice bob sue tom)])
;;   (newline)
;;   (displayln (list-ref people id))
;;   (show-freq (repeat (sample id) 1000))
;;   )

(show-marginals (sample) (list "strength alice"
                               "strength bob"
                               "strength sue"
                               "strength tome"))
