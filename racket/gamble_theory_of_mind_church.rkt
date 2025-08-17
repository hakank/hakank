#| 

  Theory of Mind (Church) in Racket/Gamble 

  These adapted Church models are from https://cbmm.mit.edu/sites/default/files/documents/CBMM_Church_Notes.html




  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


#|
  Deciding which action to take given a goal and choice of actions.

  Output: '(left left left left left left left left left left)

|#
;; a function for planning
(define (choose-action1 goal? transition state)
  (rejection-query
   ; mh-query 
   (define action (action-prior))
   ;; what action to take?
   action
   ;; conditioned on achieving the goal
   #:when
   (goal? (transition state action))))

;; without any specific goal, choose a uniform prior
(define (action-prior) (uniform-draw '(left right)))

;; A simple world, apple on the left, banana on the right
(define (lift-box state action)
  (case action
        ((left) 'apple)
        ((right) 'banana)
        (else 'nothing)))

; a simple goal
(define (got-apple? state)
  (equal? state 'apple))

(repeat (lambda() (choose-action1 got-apple? lift-box 'start)) 10)


#|
 
  Infering an agents goal from its infered action

  Output: '(banana banana banana banana banana banana banana banana banana banana)

|#
(define (choose-action2 goal? transition state)
  (rejection-query
   (define action (action-prior))
   ;; what action to take?
   action
   ;; conditioned on achieving the goal
   #:when
   (goal? (transition state action))))

;; without any specific goal, choose a uniform prior
;; (define (action-prior) (uniform-draw '(left right)))

;; A simple world, apple on the left, banana on the right
;; (define (lift-box state action)
;;   (case action
;;         (('left) 'apple)
;;         (('right) 'banana)
;;         (else 'nothing)))

(define observed-action 'right)

(define (infer-goal)
 (rejection-query
  (define goal-food (uniform-draw '(banana apple)))
  (define goal? (lambda (outcome) (equal? outcome goal-food)))
  
  goal-food
  #:when
  (equal? (choose-action2 goal? lift-box 'start) observed-action)))

(repeat (lambda() (infer-goal)) 10)


;; #|
;;   Infering an agents beliefs about the world if you know their goals and observe their actions

;;   Output: '((banana apple) (banana apple) (banana apple) (banana apple))
;; |#

;; a function for planning
(define (choose-action3 goal? transition state world)
  (rejection-query
   (define action (action-prior))
   ;; what action to take?
   action
   ;; conditioned on achieving the goal
   #:when
   (goal? (transition state action world))))

;; without any specific goal, choose a uniform prior
; (define (action-prior) (uniform-draw '(left right)))

;; the agent likes apples
;; (define (got-apple? state)
;;   (equal? state 'apple))

;; A simple world, but we don’t where where the food is
(define worldA '(apple banana))
(define worldB '(banana apple))

(define (lift-box3 state action world)
  (case action
        ((left) (first world))
        ((right) (second world))
        (else 'nothing)))

; (define observed-action 'right)

(define (infer-belief3)
  (; mh-query 4 1
   rejection-query
   (define world (if (flip) worldA worldB))
   ; query on what world the agent thinks we're in
   world
   
   ; conditioned on their action
   #:when
   (equal? 
    (choose-action3 got-apple? lift-box3 'start world) 
    observed-action)))

(repeat (lambda () (infer-belief3)) 4)
