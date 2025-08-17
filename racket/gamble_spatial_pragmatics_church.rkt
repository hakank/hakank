#| 

  Spatial pragmatics (Church) in Racket/Gamble 

  Slightly adapted Church model from 
  https://cbmm.mit.edu/sites/default/files/documents/CBMM_Church_Notes.html
  """
  Here we apply the Rational Speech Act model
  [Noah D. Goodman, Andreas Stuhlmüller:
  "Knowledge and Implicature: Modeling Language Understanding as Social Cognition"
  http://onlinelibrary.wiley.com/doi/10.1111/tops.12007/full]
  to spatial pragmatics – given the question "Where is Josh?" a speaker will
  answer with the statement that gives the listener the best chance of inferring
  the speaker’s intention.
  """

  Here's the output from two runs (40 points)

  First run:

0.35081 |          *                  
        |       *                *    
        |           *               **
        |         *     *    *        
        |     *    **   *          ** 
        |                             
        |           *         *       
        |             *         * *   
        |                *            
        |      *                      
        |               *    *  *     
        |           **    *  ** *     
        |         *                   
        |       *          *          
        +------*-------*---*----------  0.93515
        0


  Second run:

0.3655 |                      *      
       |                             
       |      *  **  *           *   
       |       *        *            
       |       *    ** *   *         
       |           *             *   
       |          *     *     ** *   
       |           *    *  * **      
       |      *  **     *            
       |                *           *
       |          *          *       
       |       *              *      
       |                  *       *  
       |        *         *          
       +------------*----------------  0.97023
       0


  100 points

0.36356 |           *                 
        |              *   *  * *   **
        |        * ** * *      ***    
        |      **     * *   **   ** * 
        |      *   * *   *   *        
        |        *  ** **  *     *    
        |     *   *  *   *        *   
        |       *      *   **      *  
        |     *  *    * **     *  *   
        |      *  *  *     *   *  *   
        |     ** *      * **          
        |      *        *    **       
        |       * * * *     *         
        |      **  * *    * ***       
        +-------*------*------------*-  0.97402
        0


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; Prior belief is uniform across the map
(define (state-prior) 
  (list (uniform 0 1) (uniform 0 1)))

;; Euclidean distance function
(define (dist p1 p2) 
  (sqrt (+ (pow (- (first p1) (first p2)) 2) 
           (pow (- (second p1) (second p2)) 2))))

;; All sentences are equally likely a priori
(define (sentence-prior) 
  (uniform-draw (list in-woods-hole in-town in-pie)))

;; Defining which sentences are true
(define (in-woods-hole state) 
  (or (< (first state) 1) (< (second state) 1)))

(define (in-town state) 
  (and (> (first state) .25) (< (second state) .33)))

(define (in-pie state)
  (and (> (first state) .833) (< (second state) .167)))

;; Speaker model
(define (speaker state depth)
  (rejection-query
   (define words (sentence-prior))
   words
   #:when
   (< (dist state (listener words depth)) 0.05)))

;; Listener model
(define (listener words depth)
  (rejection-query
   (define state (state-prior))
   state
   #:when
   (if (= depth 0)
       (words state)
       (equal? words (speaker state (- depth 1))))))

;; Run the model
(define depth 1)

;; It takes about 8.7s to generate 100 data points
(define sample (repeat (lambda () (listener in-town depth)) 100))
;; And 4.6s to generate 40 data points
;; (define sample (repeat (lambda () (listener in-town depth)) 40))



(plot-2d sample 30 15)
