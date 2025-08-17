#| 

  Hyperbole (Church, forestdb) in Racket/Gamble 

  From http://forestdb.org/models/hyperbole.html
  """
  A model of hyperbole understanding as pragmatic reasoning:

  The speaker chooses an utterance conditioned on the listener inferring information 
  that is correct and relevant to the speaker’s communicative goal (or QUD). The goal 
  can either be to communicate the state of the world, the speaker’s attitude towards 
  the state of the world (affect), or both. The listener chooses an interpretation 
  conditioned on the speaker selecting the given utterance when intending to 
  communicate this meaning. In this example the state of the world is how much an 
  electric kettle cost.
  """

  Output:
(50 0)   : ######## (0.024908029665478858)
(50 1)   : ###################################################################### (0.22551481417673835)
(51 0)   : ######## (0.022894062938662495)
(51 1)   : ################################################################# (0.20728056047398188)
(500 0)  : # (0.0009619090600546676)
(500 1)  : ####################### (0.07134957565735535)
(501 0)  : # (0.0009709325972033983)
(501 1)  : ####################### (0.07201889625451624)
(1000 0) : # (0.0002064489858371227)
(1000 1) : ########### (0.033669851501510334)
(1001 0) : # (0.00019533962337055092)
(1001 1) : ########## (0.03185802092743803)
(5000 0) : # (4.625603966087683e-5)
(5000 1) : ###### (0.018029198210717183)
(5001 0) : # (4.584303930676186e-5)
(5001 1) : ###### (0.017868223226692927)
(10000 0): # (0.0020797814881084037)
(10000 1): ################################################### (0.16397287337339939)
(10001 0): # (0.0011852372662467324)
(10001 1): ################################# (0.10494414549372028)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

; (require "gamble_distributions.rkt")


;; Define list of kettle prices under consideration (possible price states)
(define states
  (list 50 51 500 501 1000 1001 5000 5001 10000 10001))


;; Prior probability of kettle prices (taken from human experiments)
(define (state-prior) 
  (multinomial2 states 
               '(0.4205 0.3865 0.0533 0.0538 0.0223 0.0211 0.0112 0.0111 0.0083 0.0120)))

;; Probability that given a price state, the speaker thinks it's too
;; expensive (taken from human experiments)
(define (valence-prior state)
  (if (flip (second (assoc state
                           (list (list 50 0.3173)
                                 (list 51 0.3173)
                                 (list 500 0.7920)
                                 (list 501 0.7920)
                                 (list 1000 0.8933)
                                 (list 1001 0.8933)
                                 (list 5000 0.9524)
                                 (list 5001 0.9524) 
                                 (list 10000 0.9864)
                                 (list 10001 0.9864)))))
      1
      0))

;; Uniform prior over QUDs
;; (de-refernce through qud name since mem doesn't play nice with function values)
;; (define (qud-prior)
;;   (uniform-draw (list 's 'v 'sv 'as 'asv)))

(define (qud-prior)
  (multinomial2 (list 's 'v 'sv 'as 'asv) '(0.17 0.32 0.17 0.17 0.17)))

(define (qud-fn qud)
  (second
   (assoc qud
          (list
           (list 's (lambda (state valence) state))
           (list 'v (lambda (state valence) valence))
           (list 'sv (lambda (state valence) (list state valence)))
           (list 'as (lambda (state valence) (approx state 10)))
           (list 'asv (lambda (state valence) (list (approx state 10) valence)))))))

;; Round x to nearest multiple of b (used for approximate interpretation):
(define (approx x b) (* b (round (/ x b))))

;; Define list of possible utterances (same as price states)
(define utterances states)

;; Sharp numbers are costlier
(define (utterance-prior)
  (multinomial2 utterances
               '(0.18 0.1 0.18 0.1 0.18 0.1 0.18 0.1 0.18 0.1)))

;; Literal interpretation "meaning" function, just check if uttered number reflects price state
(define (literal-interpretation utterance state)
  (equal? utterance state))

;; Pragmatic listener, jointly infers the price state, speaker valence, and QUD
(define prag-listener
  (mem
   (lambda (utterance)
     (enumeration-query
      (define state (state-prior))
      (define valence (valence-prior state))
      (define qud (qud-prior))
      (define val ((qud-fn qud) state valence))
      (list state valence)
      #:when
      (equal? utterance
              (apply multinomial2 (speaker val qud)))))))

;; Speaker, chooses an utterance to convey a particular value of the qud
(define speaker
  (mem
   (lambda (val qud)
     (enumeration-query
      (define utterance (utterance-prior))
      utterance
      #:when
      (equal? val (apply multinomial2 (lit-listener utterance qud)))))))

;; Literal listener, infers the qud value assuming the utterance is true of the state
(define lit-listener
  (mem
   (lambda (utterance qud)
     (enumeration-query
      (define state (state-prior))
      (define valence (valence-prior state))
      ((qud-fn qud) state valence)
      #:when
      (literal-interpretation utterance state)))))

(barplot (prag-listener 10000))

