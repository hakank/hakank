#| 

  Scalar implicature in Racket Gamble.

  This is a port of Church's model Scalar implicature 
  (http://forestdb.org/models/scalar-implicature.html)
  Part 1

  
  (1 : 0.46496051184644605)
  (2 : 0.4326701989403179)
  (3 : 0.10236928921323603)
  (mean: 1.6375087473757872)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)

(require "gamble_utils.rkt")


;;; """
;;; A model of pragmatic language interpretation:
;;;
;;; The speaker chooses a sentence conditioned on the listener inferring the intended
;;; state of the world when hearing this sentence; the listener chooses an
;;; interpretation conditioned on the speaker selecting the given utterance when
;;; intending this meaning.
;;; """

(define (state-prior) (uniform-draw '(0 1 2 3)))

(define (sentence-prior) (uniform-draw (list all-sprouted some-sprouted none-sprouted)))

(define (all-sprouted state) (= 3 state))
(define (some-sprouted state) (< 0 state))
(define (none-sprouted state) (= 0 state))

(define (speaker state depth)
  (rejection-query
   (define words (sentence-prior))
   words
   #:when
   (equal? state (listener words depth))))

(define (listener words depth)
  (rejection-query
   (define state (state-prior))
   state
   #:when
   (if (= depth 0)
       (words state)
       (equal? words (speaker state (- depth 1))))))

(define depth 1)

(show-freq (repeat (lambda () (listener some-sprouted depth)) 10000))
