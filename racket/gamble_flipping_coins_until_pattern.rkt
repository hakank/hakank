#| 

  Flipping coins until HTTTH or HTHTHT in Racket.Gamble 
 
  From "Perplexing the Web, One Probability Puzzle at a Time" 
  https://www.quantamagazine.org/perplexing-the-web-one-probability-puzzle-at-a-time-20240829/?mc_cid=94caee8978
  (about Daniel Litt)
  """
  Puzzle 2
 
  You keep flipping a fair coins until you see five consecutive flips of the
  form HTTTH or HTHTH (where H means heads and T means tails). Then stop.
  Those last five flips are more likely to have been
  - HTTTH
  - HTHTH
  - equally likely
  """

  The problem was originally stated in Daniel Litt's
  https://x.com/littmath/status/1786115416231641335 (May 2, 2024)
  """
  Keep flipping a fair coin until you see five consecutive flips of the form 
  HTTTH or HTHTH (where H means heads and T means tails), then stop. 
  Your last 5 flips are more likely to have been:
  HTTTH  15.2%
  HTHTH  14.5%
  Equally likely 53.5%
  Don't know/see results 16.7%
  """

  Here we code H -> 1, T -> 0 

  Enumerate does not work here, so importance-sampler is used (40000 samples)

  var : found
  (1 0 0 0 1): 0.5540999999999998
  (1 0 1 0 1): 0.44589999999999874

  var : len
  6: 0.06379999999999987
  5: 0.06239999999999988
  7: 0.054724999999999885
  8: 0.0530749999999999
  9: 0.049074999999999896
  ...
  152: 2.499999999999995e-5
  158: 2.499999999999995e-5
  159: 2.499999999999995e-5
  166: 2.499999999999995e-5
  190: 2.499999999999995e-5
  mean: 19.815324999999955


  Some other patterns: 

  * (1 0 1) vs (1 1 1)

    var : found
    (1 0 1): 0.5996249999999997
    (1 1 1): 0.4003749999999997

    var : len
    mean: 6.837924999999994


  * (1 1 0 0) vs (1 0 1 0)

    var : found
    (1 1 0 0): 0.5573750000000002
    (1 0 1 0): 0.4426249999999997

    var : len
    mean: 8.857824999999998

  * experimental: unequal lengths of pattern

    We have four patterns
                   (1 0 1 1)
                   (1 1 1 0)  <-- This can not be reached
                   (1 1)
                   (0 0)
    but only three of them can be found, since one of 
    the smaller subsumbes the larger pattern. Here (1 1 1 0) will not
    be found.

    var : found
    (0 0): 0.4987999999999999
    (1 1): 0.37635
    (1 0 1 1): 0.12485000000000002

    var : len
    mean: 3.0003999999999995


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define patterns '(
                   (1 0 0 0 1) ; HTTTH
                   (1 0 1 0 1) ; HTHTH
                   ))

;;
;; Some other patterns (see above)
;;

;; HH vs HT
;; (define patterns '(
;;                    (1 1)
;;                    (1 0)))


;; (define patterns '(
;;                    (1 0 1)
;;                    (1 1 1)))

;; (define patterns '(
;;                    (1 1 0 0)
;;                    (1 0 1 0)
;;                    ))

;; Experimental: unequal lengths
;; (Should be in decreasing length)
;; (define patterns '(
;;                    (1 0 1 1)
;;                    (1 1 1 0)                   
;;                    (1 1)
;;                    (0 0)
;;                    ))

(define (model)
  (; enumerate #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   #|
   (define pattern-len (length (first patterns)))

   ; Assumes equal lengths of the patterns
   (define (a lst)
     (let ([len (length lst)]
           [new (list (bernoulli 0.5))])
       ; last five flips
       (if (>= len min-pattern-len)
           (let* ([s  (list-slice lst (- len pattern-len))]
                  [p-check (for/first ([p patterns]
                                      #:when (equal? s p))
                             (list p len))])
             ; did we found a pattern?
             (if (list? p-check)
                 p-check ; return result
                 (a (append lst new)) ; continue
                 )
             )
           ; Not yet of appropriate length
           (a (append lst new))
           ))
     )
    |#

   (define min-pattern-len (apply min (map (lambda (v) (length v)) patterns)))
   
   ; Handle patterns of different lengths (experimental)
   (define (a lst)
     (let ([lst-len (length lst)]
           [new (list (bernoulli 0.5))])
       ; check last flips
       (if (>= lst-len min-pattern-len)
           (let ([p-check (for/first ([p patterns]
                                      #:do [(define p-len (length p))
                                            (define ok? (>= (- lst-len p-len) 0)) ; approriate lengh
                                            (define s (if ok?
                                                          (list-slice lst (- lst-len p-len))
                                                          #f))]
                                      #:when (and ok? (equal? s p)))
                            (list p lst-len))
                          ])
             ; did we found a pattern?
             (if (list? p-check)
                 p-check ; return result
                 (a (append lst new)) ; continue
                 )
             )
           ; Not yet of appropriate length
           (a (append lst new))
           )
       )
     )
   
   (define res (a '()))
   (define found (first res))
   (define len (second res))
   
   (list found
         len
         )
   
   )
             
)

(show-marginals (model)
                (list  "found"
                       "len"
                       )
                #:num-samples 40000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
