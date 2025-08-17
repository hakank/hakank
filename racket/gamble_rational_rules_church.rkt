#| 

  Rational rules (Churs) in Racket/Gamble 

  This is a slightly adapted version of the Church model
  http://forestdb.org/models/rational-rules.html
  """
  Rational Rules is a model of concept learning. Concepts are represented as compositional
  rule formulas. The version below is a simplified version of the model used in 
  Goodman et al. (2008).
  [http://www.stanford.edu/~ngoodman/papers/op322-goodman.pdf]
  ...
  References:
  
  A rational analysis of rule-based concept learning. Goodman, Noah D and Tenenbaum,
  Joshua B and Feldman, Jacob and Griffiths, Thomas L (2008). Cognitive Science.
  [http://www.stanford.edu/~ngoodman/papers/op322-goodman.pdf]
  Probabilistic Models of Cognition. Noah D. Goodman and Joshua B. Tenenbaum (2011).
  [https://probmods.org/]
  """

  Output:

0.83 |                                     * 
     |                                      *
     |                                  *    
     |                                       
     |                                       
     |                       *               
     |                        *              
     |                     **                
     |                                       
     |                                       
     |                 **                    
     |                 *                     
     |         *                             
     |                                       
     |                                       
     |  *  *                                 
     |*                                      
     |                                       
     |                                       
     +---------------------------------------  0.964
     0



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


;;first set up the training (cat A/B) and test objects:
(define num-features 4)

(define A-objects (list '(0 0 0 1) '(0 1 0 1) '(0 1 0 0) '(0 0 1 0) '(1 0 0 0)))

(define B-objects (list '(0 0 1 1) '(1 0 0 1) '(1 1 1 0) '(1 1 1 1)))

(define T-objects (list '(0 1 1 0) '(0 1 1 1) '(0 0 0 0) '(1 1 0 1)
                        '(1 0 1 0) '(1 1 0 0) '(1 0 1 1)))

;;here are the human results from Nosofsky et al, for comparison:
(define human-A '(0.77 0.78 0.83 0.64 0.61))
(define human-B '(0.39 0.41 0.21 0.15))
(define human-T '(0.56 0.41 0.82 0.40 0.32 0.53 0.20))

;;two parameters: stopping probability of the grammar, and noise probability:
(define tau 0.3)         
(define noise-param (exp -1.5)) 

;;a generative process for disjunctive normal form propositional equations:
(define (get-formula)
  (if (flip tau)
      (let ((c (Conj))
            (f (get-formula)))
        (lambda (x) (or (c x) (f x))))
      (Conj)))

(define (Conj)
  (if (flip tau)
      (let ((c (Conj))
            (p (Pred)))
        (lambda (x) (and (c x) (p x))))
      (Pred)))

(define (Pred)
  (let ((index (sample-integer num-features))
        (value (sample-integer 2)))
    (lambda (x) (= (list-ref x index) value))))


(define (noisy-equal? a b) (flip (if (equal? a b) 0.999999999 noise-param)))

(define (samples)
  (; mh-query  ; getting a strange error: "collision in random choice database"
   ; 1000 10
   rejection-query
   
   ;;infer a classification formula
   (define my-formula (get-formula))

   ;;look at posterior predictive classification
   (map my-formula (append T-objects A-objects B-objects))
   
   ;;conditioning (noisily) on all the training eamples:
   #:when
   (and (all (map (lambda (x) (noisy-equal? true (my-formula x))) A-objects))
        (all (map (lambda (x) (noisy-equal? false (my-formula x))) B-objects)))))


;;now plot the predictions vs human data:
(define (means samples)
  (if (null? (first samples))
      '()
      (pair (mean (map (lambda (x) (if x 1.0 0.0)) (map first samples)))
            (means (map rest samples)))))

(define the-samples (repeat (lambda () (samples)) 1000))

(scatter (map list (means the-samples) (append human-T human-A human-B)))

