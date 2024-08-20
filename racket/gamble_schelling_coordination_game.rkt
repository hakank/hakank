#| 

  Schelling coordination game in Racket Gamble.

  This is a port of the Church ForestDB model, http://forestdb.org/models/schelling.html

  It uses rejection-query from Gamble's church-compat.rkt 
  (gamble/examples/forestdb/church-compat.rkt).

    (good-bar : 0.7734530938123753)
    (bad-bar : 0.22654690618762474)

  The model is almost verbatin from the Church model, with the exception that one
  have to add #:when for the observation part (see below).

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)

(require "gamble_utils.rkt")

;;;
;;; This works as expected, but note that this use 
;;; Church's rejection-query, not Gamble's rejection-sampler!
;;; (good-bar : 0.7634730538922155)
;;; (bad-bar : 0.23652694610778444)
;;;
;;; And note the #:when in the observe part
;;;
(define (sample-location)
  (if (flip .6)
      'good-bar
      'bad-bar))

(define (alice depth)
  (rejection-query
   (define alice-location (sample-location))
   alice-location
   #:when ; <---
   (equal? alice-location (bob (- depth 1)))))

(define (bob depth)
  (rejection-query
   (define bob-location (sample-location))
   bob-location
   #:when ; <---
   (if (= depth 0)
       #t
       (equal? bob-location (alice depth)))))

(displayln "sampler")
(show-freq (repeat (lambda () (bob 1)) 1000))
