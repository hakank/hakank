#| 

  Hidden Markov Model in Racket Gamble.

  This is from the BLOG distribution hmm.dblog
  """
  A hidden Markov model with four states and four output symbols.
  The parameters of this model are completely made-up.
  DBLOG model
  """

var : (S 0)
C: 0.8160000000000006
T: 0.08400000000000005
A: 0.07900000000000004
G: 0.020999999999999994

var : (S 1)
A: 0.8740000000000007
G: 0.049000000000000016
T: 0.03900000000000001
C: 0.038000000000000006

var : (S 2)
A: 0.8530000000000006
T: 0.05600000000000002
C: 0.04600000000000001
G: 0.04500000000000001

var : (S 3)
A: 0.8450000000000006
G: 0.05500000000000002
T: 0.05100000000000002
C: 0.049000000000000016

var : (S 4)
G: 0.8370000000000006
A: 0.07300000000000004
C: 0.04500000000000001
T: 0.04500000000000001

var : (S 5)
A: 0.2960000000000002
G: 0.2590000000000002
C: 0.22600000000000017
T: 0.21900000000000017

var : (S 6)
A: 0.2880000000000002
G: 0.2570000000000002
T: 0.22800000000000017
C: 0.22700000000000017

var : (S 7)
A: 0.2880000000000002
G: 0.2770000000000002
C: 0.21800000000000017
T: 0.21700000000000016

var : AA
mean: 0.5751610358409813

var : AC
mean: 0.4631908561202461

var : AG
mean: 0.4935112298976939

var : AT
mean: 0.4722570975901266

var : CA
mean: 0.5643226371519511

var : CC
mean: 0.4664798405973259

var : CG
mean: 0.5048432673449625

var : CT
mean: 0.4791088264613955

var : GA
mean: 0.5511796290910252

var : GC
mean: 0.47630970506659115

var : GG
mean: 0.49951130640375196

var : GT
mean: 0.4710329701776746

var : TA
mean: 0.5665465297726168

var : TC
mean: 0.46870516144569746

var : TG
mean: 0.5009647958208047

var : TT
mean: 0.45281336340894474


  This is a port of my WebPPL model hmm1.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (hmm1)
  (; enumerate
   rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define states (vector "A" "C" "G" "T"))
    
   (define AA (uniform 0 1))
   (define AC (uniform 0 1))
   (define AG (uniform 0 1))
   (define AT (uniform 0 1))
   
   (define CA (uniform 0 1))
   (define CC (uniform 0 1))
   (define CG (uniform 0 1))
   (define CT (uniform 0 1))
   
   (define GA (uniform 0 1))
   (define GC (uniform 0 1))
   (define GG (uniform 0 1))
   (define GT (uniform 0 1))
   
   (define TA (uniform 0 1))
   (define TC (uniform 0 1))
   (define TG (uniform 0 1))
   (define TT (uniform 0 1))

   (define (S1 t)
     (if (= t 0)
         (categorical-vw2 (vector 0.3 0.2 0.1 0.4) states)
         (begin
           (let ([prev_t (S1 (sub1 t))])
             (cond
               [(eq? prev_t "A") (categorical-vw2 (vector AA AC AG AT) states)]
               [(eq? prev_t "C") (categorical-vw2 (vector CA CC CG CT) states)]
               [(eq? prev_t "G") (categorical-vw2 (vector GA GC GG GT) states)]
               [(eq? prev_t "T") (categorical-vw2 (vector TA TC TG TT) states)]
               [else "xxx"])))))
   
   (define S (mem (lambda (t)
                    (S1 t))))
   
   (define results (vector "ResultA" "ResultC" "ResultG" "ResultT"))
   
   (define (O t) 
     (define St (S t))
     (cond
       [(eq? St "A") (categorical-vw2 (vector 0.85 0.05 0.05  0.05) results)]
       [(eq? St "C") (categorical-vw2 (vector 0.05 0.85 0.05  0.05) results)]
       [(eq? St "G") (categorical-vw2 (vector 0.05 0.05 0.85  0.05) results)]
       [(eq? St "T") (categorical-vw2 (vector 0.05 0.05 0.05  0.85) results)]
       [else "yyy"]))
   
    
    ;; /* Evidence for the Hidden Markov Model.
    ;;  */
    
    (observe/fail (eq? (O 0) "ResultC"))
    (observe/fail (eq? (O 1) "ResultA"))
    (observe/fail (eq? (O 2) "ResultA"))
    (observe/fail (eq? (O 3) "ResultA"))
    (observe/fail (eq? (O 4) "ResultG"))
    
    ;; /* Queries for the Hiddem Markov Model, given the evidence.  
    ;;  * Note that we can query S(5) even though our observations only 
    ;;  * went up to time 4.
    ;;  */

    (list (S 0)
          (S 1)
          (S 2)
          (S 3)
          (S 4)
          (S 5) ;; The unknown
          (S 6) ;; The unknown
          (S 7) ;; The unknown
          AA AC AG AT 
          CA CC CG CT           
          GA GC GG GT 
          TA TC TG TT 
    )
         
   )
  )

(show-marginals (hmm1)
                (list "(S 0)"
                      "(S 1)"
                      "(S 2)"
                      "(S 3)"
                      "(S 4)"
                      "(S 5)" ;; The unknown
                      "(S 6)" ;; The unknown
                      "(S 7)" ;; The unknown
                      
                      "AA"
                      "AC"
                      "AG"
                      "AT"
                      
                      "CA"
                      "CC"
                      "CG"
                      "CT"
                      
                      "GA"
                      "GC"
                      "GG"
                      "GT"
                      
                      "TA"
                      "TC"
                      "TG"
                      "TT"
                      )
                #:num-samples 1000
                #:truncate-output 4
                ; #:skip-marginals? #t
                )

