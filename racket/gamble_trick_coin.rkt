#| 

  Trick coin in Racket Gamble.

  From church 
  """
;; 5.1 The trick coin

(define observed-data '(h h h h h h h))
(define num-flips (length observed-data))

(define samples
    (mh-query
        1000 10

        (define fair-prior 0.999)
        (define fair-coin? (flip fair-prior))

        (define make-coin (lambda (weight) (lambda () (if (flip weight) 'h 't))))
        (define coin (if fair-coin?
                      (make-coin 0.5)
                      (make-coin 0.95)))
                     ;(make-coin (if fair-coin? 0.5 0.95)))
        (define sampled-data (repeat num-flips coin))
        fair-coin?

        (equal? observed-data sampled-data)))

  ;;; (hist samples "Fair coin?")

  """
  
  * rejection-sampler
    (#f : 0.9912)
    (#t : 0.0088)

  * enumerate (exact)
    (#f : 0.9921875)
    (#t : 0.007812500000000007)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

(define observed-data '(h h h h h h h))
(define num-flips (length observed-data))


; (discrete-dist [#f 0.9914] [#t 0.0086])
(define samples
  (rejection-sampler
   
   (define fair-prior 0.999)
   (define fair-coin? (flip fair-prior))
   (define make-coin (lambda (weight) (lambda () (if (flip weight) 'h 't))))
   
   (define coin (if fair-coin?
                    (make-coin 0.5)
                    (make-coin 0.95)))
                   ;(make-coin (if fair-coin? 0.5 0.95)))
   (define sampled-data (repeat coin num-flips)) ; Gamble has reverse order of repeat than Church!
   ;; fair-coin?
   (observe/fail fair-coin?)
   (equal? observed-data sampled-data)
   )
  )

(displayln "rejection-sampler")
(show-model samples #:num-samples 10000)
(newline)

; Exact probabilities with enumerate
; -> (discrete-dist [#f 0.9921875] [#t 0.007812500000000007])
(displayln "enumerate:")
(show-model
 (enumerate
  (define fair-prior 0.999)
  (define fair-coin? (flip fair-prior))
  (define make-coin (lambda (weight) (lambda () (if (flip weight) 'h 't))))
  
  (define coin (if fair-coin?
                   (make-coin 0.5)
                   (make-coin 0.95)))
  ;(make-coin (if fair-coin? 0.5 0.95)))
  (define sampled-data (repeat coin num-flips)) ; Gamble has reverse order of repeat than Church!
  ;; fair-coin?
  ; (observe/fail (eq? fair-coin? #t))
  (observe/fail fair-coin?)
  (equal? observed-data sampled-data)
  )
 )
