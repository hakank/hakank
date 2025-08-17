#| 

  Hyperbolic metaphor (Church, forestdb) in Racket/Gamble 

  From http://forestdb.org/models/hyperbolicmetaphor.html
  * John could either be a giraffe or a person.
  * It is extremely unlikely that John is actually a giraffe.
  * The speaker could either say "John is a giraffe" or "John is a person."
  * The speaker says "John is a giraffe"

  What is the probability that John is a person/giraffe, and what is the
  probability of John's height?


  Output:

(giraffe 160): # (6.172839506172843e-6)
(giraffe 180): # (6.172839506172843e-6)
(giraffe 200): # (3.086419753086419e-5)
(giraffe 220): # (3.086419753086419e-5)
(giraffe 240): # (6.172839506172838e-5)
(giraffe 260): # (6.172839506172838e-5)
(giraffe 280): # (0.0004938271604938272)
(giraffe 300): # (0.00030864197530864224)
(person 160) : ###################################################################### (0.4036363636363637)
(person 180) : ###################################################################### (0.4036363636363637)
(person 200) : ################## (0.1009090909090909)
(person 220) : ######### (0.05045454545454545)
(person 240) : ## (0.01009090909090909)
(person 260) : ## (0.01009090909090909)
(person 280) : ## (0.01009090909090909)
(person 300) : ## (0.01009090909090909)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;; John could either be a giraffe or a person.
(define categories (list 'giraffe 'person))

;; It is extremely unlikely that John is actually a giraffe.
(define (categories-prior) (multinomial2 categories '(0.001 0.999)))

;; The speaker could either say "John is a giraffe" or "John is a person."
(define utterances (list 'giraffe 'person))

;; The utterances are equally costly.
(define (utterance-prior) (multinomial2 utterances '(0.1 0.1)))

;; The only feature being considered is height
(define heights (list 160 180 200 220 240 260 280 300)) 
(define height-prior 
  (list
   (list 0.01 0.01 0.05 0.05 0.1 0.1 0.8 0.5)
   (list 0.4 0.4 0.1 0.05 0.01 0.01 0.01 0.01)
   ))

;; Speaker's possible goals are to communicate whether John is tall or his actual height
(define goals (list 'tall? 'height?))
(define threshold 180)

;; Prior probability of speaker's goal is set to uniform but can
;; change with context/QUD.
(define (goal-prior) (uniform-draw goals))

;; Speaker optimality parameter
(define alpha 1)

;; Sample John's height given that he is a member of category
(define (sample-height category prior all-categories)
  (if (equal? category (first all-categories))
      (multinomial2 heights (first prior))
      (sample-height category (rest prior) (rest all-categories))))

;; Check if interpreted categroy is identical to utterance
(define (literal-interpretation utterance category)
  (equal? utterance category))

;; Check if goal is satisfied
(define (goal-satisfied? goal listener-category-height speaker-category-height)
  (case goal
    (('category?) (equal? (first listener-category-height) (first speaker-category-height)))
    (('tall?) (equal? (> (second listener-category-height) threshold) 
                      (> (second speaker-category-height) threshold)))
    (('height?) (equal? (second listener-category-height) (second speaker-category-height)))
    ))

;; Speaker model
(define speaker
  (mem
   (lambda (category height goal depth)
     (enumeration-query
      (define utterance (utterance-prior))
      utterance
      #:when
      (goal-satisfied? goal
                       (apply multinomial2 (listener utterance depth))
                       (list category height))))))

;; Listener model
(define listener
  (mem 
   (lambda (utterance depth)
     (enumeration-query
      (define category (categories-prior))
      (define height (sample-height category height-prior categories))
      (define speaker-goal (goal-prior))
      (list category height)
      #:when
      (if (equal? depth 0)
          (literal-interpretation utterance category)
          (equal? utterance
                  (apply multinomial2
                         (raise-to-power (speaker category height speaker-goal (- depth 1))
                                         alpha))))))))

(define (raise-to-power speaker-dist alpha)
  (list (first speaker-dist)
        (map (lambda (x) (pow x alpha)) (second speaker-dist))))

;; Recursive depth
(define depth 1)

(define (sample-one utterance)
  (listener utterance depth))
(barplot (sample-one 'giraffe))
