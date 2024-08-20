#| 

  Continuous weight example in Racket Gamble.

  From Church
  """
;; 6.1 Planning as inference

;; A function for planning
(define (choose-action goal? transition state)
    (rejection-query
         (define action (action-prior))
         ;; what action to take?
         action
         ;; conditioned on achieving the goal
         (goal? (transition state action))))

;; Without any specific goal, choose a uniform prior
(define (action-prior) (uniform-draw '(left right)))

;; A simple world, apple on the left, banana on the right
(define (transition-function state action)
    (case action
         (('left) 'apple)
         (('right) 'banana)
         (else 'nothing)))

; A simple goal
(define (got-apple? state) 
    (equal? state 'apple))

(choose-action got-apple? transition-function 'start)
  """
  
  Output from this model:

  '(left left left left left left left left left left)
  (left : 1.0)

  See gamble_planning_as_inference2.rkt for another version using 
  rejection-query instead of rejection_sampler.



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; A function for planning
(define (choose-action goal? transition state)
  (
   rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define action (action-prior))
   
   (observe/fail (goal? (transition state action)))

   action)
  )

;; Without any specific goal, choose a uniform prior
(define (action-prior) (uniform-draw '(left right)))

;; A simple world, apple on the left, banana on the right
(define (transition-function state action)
    (case action
         ('left 'apple)
         ('right 'banana)
         (else 'nothing)))

; A simple goal
(define (got-apple? state) 
    (equal? state 'apple))

(repeat (choose-action got-apple? transition-function 'start) 10)
(show-freq (repeat (choose-action got-apple? transition-function 'start) 1000))

