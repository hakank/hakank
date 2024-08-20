#| 

  Scalar implicature, part 2 in Racket Gamble.

  This is a port of Church's model Scalar implicature 
  (http://forestdb.org/models/scalar-implicature.html)
  Part 2.

  Without full knowledge
  (2 : 0.5557768924302788)
  (3 : 0.32768924302788843)
  (1 : 0.11354581673306773)
  (0 : 0.00298804780876494)
  (mean: 2.2091633466135456)
  
  With full knowledge
  (2 : 0.5663010967098704)
  (3 : 0.27916251246261214)
  (1 : 0.15453639082751744)
  (mean: 2.1256231306081754)


  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)

(require "gamble_utils.rkt")


;;;
;;; """
;;; A more complex version of the model takes into account the listener’s knowledge
;;; about the speaker’s access to the items the speaker is referring to. In this
;;; version, lack of access can lead to a cancelled implicature (i.e. “some” does
;;; not imply “not all”):
;;; """
;;;
(define (belief actual-state access)
  (map (lambda (ac st pr) (if ac st (sample pr)))
       access
       actual-state
       (substate-priors)))

(define baserate 0.8)

(define (substate-priors)
  ;; (list (lambda () (flip (baserate)))
  ;;       (lambda () (flip (baserate)))
  ;;       (lambda () (flip (baserate)))))
  ; hakank: It must be a distribution, not just a list of flips
  (list (flip-dist baserate)
        (flip-dist baserate)
        (flip-dist baserate)))

(define (state-prior)
  (map sample (substate-priors)))

(define (sentence-prior)
  (uniform-draw (list all-p some-p none-p)))

; (define (all-p state) (all state))
(define (all-p state) (all state)) ; hakank
; (define (some-p state) (any state))
(define (some-p state) (any? state)) ; hakank
(define (none-p state) (not (some-p state)))

(define (speaker access state depth)
  (rejection-query
   (define sentence (sentence-prior))
   sentence
   #:when ; hakank
   (equal? (belief state access)
           (listener access sentence depth))))

(define (listener speaker-access sentence depth)
  (rejection-query
   (define state (state-prior))
   state
   #:when ; hakank
   (if (= 0 depth)
       (sentence state)
       (equal? sentence
               (speaker speaker-access state (- depth 1))))))

(define (num-true state)
  (sum (map (lambda (x) (if x 1 0)) state)))

(define (show thunk)
  (show-freq (repeat thunk 200)))

(displayln "Without full knowledge:")
(show (lambda () (num-true (listener '(#t #t #f) some-p 1))))
(newline)

;; With full knowledge:
(displayln "with full knowledge:")
(show (lambda () (num-true (listener '(#t #t #t) some-p 1))))
(newline)
