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
  
  '(left left left left left ...left left left)

  Note: This uses rejection-query, not rejection-sampler (see below)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; ;; A function for planning
;; (define (choose-action goal? transition state)
;;   (writeln (list "choose-action" "goal?" goal? "transition" transition "state" state))
;;   (
;;    rejection-sampler
;;    ; importance-sampler
;;    ; mh-sampler
   
;;    (define action (action-prior))
;;    ; (writeln (list "action" action))
   
;;    ;; what action to take?
;;    ;; conditioned on achieving the goal
;;    ; (goal? (transition state action))
   
;;    (observe/fail (goal? (transition state action)))

;;    action)
;;   )

;; ;; Without any specific goal, choose a uniform prior
;; (define (action-prior) (uniform-draw '(left right)))

;; ;; A simple world, apple on the left, banana on the right
;; (define (transition-function state action)
;;     (case action
;;          (('left) 'apple)
;;          (('right) 'banana)
;;          (else 'nothing)))

;; ; A simple goal
;; (define (got-apple? state) 
;;     (equal? state 'apple))

;; ; (choose-action got-apple? transition-function 'start)
;; ;;; (sampler->discrete-dist (choose-action got-apple? transition-function 'start) 100)
;; (show-freq (repeat (choose-action got-apple? transition-function 'start) 100))


;;; This is the original Church model with a few changes, e.g. #:when.
;; But it does not work as expected. It should just show left, not it's stuck in some infinite?
;; loop. Why?

;; Without any specific goal, choose a uniform prior
(define (action-prior) (uniform-draw '(left right)))


(define (choose-action goal? transition state)
  (rejection-query
   (define action (action-prior))
   ;; what action to take?
   action
   ;; conditioned on achieving the goal
   #:when ; hakank
   (goal? (transition state action))))

;; A simple world, apple on the left, banana on the right
(define (transition-function state action)
  (case action
    ; hakank:
    ; (('left) 'apple)  
    ; (('right) 'banana)
    ('left 'apple)
    ('right 'banana)
    (else 'nothing)))

; A simple goal
(define (got-apple? state)
  (equal? state 'apple))

(repeat (lambda () (choose-action got-apple? transition-function 'start)) 100)

