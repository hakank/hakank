#| 

  Word learning as Bayesian inference (Church) in Racket.

  From http://forestdb.org/models/word-learning.html
  """
  This is a webchurch implementation of a model in Xu and Tenenbaum (2007).
  [http://web.mit.edu/cocosci/Papers/XT-PsychRev-InPress.pdf]

  Consider the following tree:
  
           [o]       (height = 1)
            |
           [a]       (height = 0.8)
         /    \
       /       \
     [b]      [e]    (height = 0.3)
     / \     /  \
   [c] [d] [f] [g]   (height = 0)
    1  4    5   7
    2       6   8
    3           9
   ​

  Here, inner nodes represent hierarchical category labels (e.g., living thing, 
  mammal, dog). The height of a node is a proxy for the size of its extension 
  (e.g., because [o] is high, it might denote a large class, like "objects that can be 
  thought about").

  Leaves represent objects that are nameable. The inference problem is this: suppose 
  that a single label (e.g., dog) has been used to label multiple objects (e.g., 
  1, 2, and 4). which ontological category does this label map onto?
  """

  Output:

(c : 0.9720837487537388)
(b : 0.024925224327018942)
(a : 0.0029910269192422734)

a:   2 # (0.002 / 0    )
b:  24 ## (0.024 / 0.002)
c: 974 ################################################################################ (0.974 / 0.026)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define nodes '(a b c d e f g))
(define heights '((o 1) (a 0.8) (b 0.3) (e 0.3) (c 0) (d 0) (f 0) (g 0) ))
(define parents '((o #f) (a o) (b a) (e a) (c b) (d b) (f e) (g e)
                  (1 c) (2 c) (3 c) (4 d) (5 f) (6 f) (7 g) (8 g) (9 g)))
(define all-nodes (append nodes '(1 2 3 4 5 6 7 8 9)))

;; check whether q is a descendent of r
(define (is-child? q r)
	(if (equal? q #f)
      #f
      (if (equal? q r)
          #t
          (is-child? (node->parent q) r))))

(define (node->height x) (second (assoc x heights)) )
(define (node->parent x) (second (assoc x parents)) )

;; define unnormalized prior probabilities
(define weights* (map (lambda (x) (- (node->height (node->parent x))
                                     (node->height x)))
                      nodes))

;; normalize prior probabilities
(define weights
	(let ([denom (sum weights*)])
		(map (lambda (x) (/ x denom)) weights*)))

;; data that we've observed
(define data '(1 2))
(define k (length data))

(define epsilon 0.05)

(define (likelihood* x) (expt (+ (node->height x) epsilon) (- 0 k)))
(define likelihood-denom (sum (map likelihood* nodes)))

(define (likelihood h) (/ (likelihood* h) likelihood-denom))

(define (samples)
  (mh-query
   1000 10
   ;; draw hypothesis according to prior
   (define node (multinomial2 nodes weights))
   
   ;; query 
   node
   
   ;; condition
   #:when
   (if (all (map (lambda (x) (is-child? x node))
                 data))
       (flip (likelihood node))
       #f)))

;; (hist samples)
(define the-samples (repeat (lambda() (samples)) 1000))
(show-freq the-samples)
(newline)
(show-histogram the-samples)


