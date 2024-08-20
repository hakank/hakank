#| 

  London Blitz example in Racket Gamble.

  From Church
  """
  ;;;;
  ;;;; 3.5.1 London Blitz example
  ;;;;
  ;; 'random' bombing
  (define (uniform-bombing num-bomb)
     (repeat 100 (lambda () (list (uniform 0 10) (uniform 0 10)))))
  ;;; (scatter (uniform-bombing 100))

  ;;; 'cluster' bombing
  (define (target-bombing num-bomb)
    (let ((number-of-gaussians (+ 3 (random-integer 5)))
           (centers (repeat number-of-gaussians sample-gaussian-center))
           (points (map (lambda (center) (repeat (floor (/ num-bomb number-of-gaussians))
                                              (lambda () (twod-gaussian center)))) centers)))
           (fold append '() points)))

  ;;; (define num-bombs 20)
  ;;; (if (flip 0.5)
  ;;;     (scatter (uniform-bombing num-bombs))
  ;;;     (scatter (target-bombing num-bombs)))'


  """

  Well, this is perhaps not so instructive 

  (7.025509225508646 0.9129226486868949)
  (4.601002434969066 6.28816592226236)
  (8.012358037887719 4.343783860911393)
  '((6.873663428959226 9.11776701290353) (6.358203520584569 8.073004765432852) (7.131159585987332 7.158350956079178) (7.373442026511272 9.233420577883395) (6.443651070303803 8.84648598961435) (4.807907947234481 8.195247003469067) (3.945719666198805 5.809980369015882) (4.868203856229318 6.1141681351076365) (4.2041224375911606 7.253030350114945) (3.897433274240978 5.118415464065961) (3.688673444336502 5.678051527070303) (3.3690395510661233 5.3234028972346685) (3.058145930722132 3.821105061781787) (3.581881096946444 4.559310260516434) (3.7787060926480143 2.9981721699955672) (3.468484776935929 1.543196772571878) (2.236371212311465 3.3811627805577906) (4.650449209846488 3.823951383766777))



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


;; random draw center from 10x10 box
(define (sample-gaussian-center)
  (writeln (list (uniform 0 10) (uniform 0 10)))
  (list (uniform 0 10) (uniform 0 10))
  )

(define number-of-gaussians (+ 1 (discrete-uniform 10)))

;;; 2D Gaussian, sample around the center
(define (twod-gaussian center)
  (list (normal (first center) 1.0) (normal (second center) 1.0)))

;;; Sample the Gaussian centers
(define centers (repeat sample-gaussian-center number-of-gaussians))


;; 'random' bombing
(define (uniform-bombing num-bomb)
  (repeat (lambda () (list (uniform 0 10) (uniform 0 10))) 100)
  )

;;; 'cluster' bombing
(define (target-bombing num-bomb)
  
  (let ((number-of-gaussians (+ 3 (discrete-uniform 5)))
        (centers (repeat (lambda () sample-gaussian-center) number-of-gaussians))
        (points (map (lambda (center) (repeat (lambda () (twod-gaussian center))
                       (floor (/ num-bomb number-of-gaussians)))
                       ) centers))
        )
    (foldr append '() points)
    )
  )

(define num-bombs 20)
(if (flip 0.5)
    (uniform-bombing num-bombs)
    (target-bombing num-bombs)
    )

; Gamble does not have scatter
;; (if (flip 0.5)
;;     (scatter (uniform-bombing num-bombs))
;;     (scatter (target-bombing num-bombs)))
