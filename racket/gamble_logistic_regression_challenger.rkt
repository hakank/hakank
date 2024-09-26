#| 

  Logistic regression - Challenger in Racket Gamble.

  From https://www.zinkov.com/posts/2012-06-27-why-prob-programming-matters/
  "Logistic Regression"
  """
  Logistic Regression can be seen as a generalization of Linear Regression where the output is 
  transformed
  to lie between 0 and 1. This model only differs from the previous one by a single line, 
  illustrating that adding this complexity does not require starting from scratch. The 
  point with probabilistic programming is you are able to explore slightly more complex models 
  very easily.
  """


  From https://www.stat.ubc.ca/~bouchard/courses/stat520-sp2014-15/lecture/2015/02/27/notes-lecture3.html
  x = 66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58
  y = 1,0,1,1,1,1,1,1,0,0,0,1,1,0,1,1,1,1,1,1,0,1,0

  The values 70 and 75 are the two valuse for which there are both true and false observations, and are
  the cause of the slightly bad prediction of about 82%.
  The p70 and p75 variables in the model are the probability that the value of 70 and 75 are 
  false, respectively. 
  The breakpoint when false switch to true is when x is around 66 (the breakpoint variable).

  var : w0
  -14.268343575118676: 0.006571617203032474
  -15.804637429952306: 0.006561543895305582
  ...
  -33.92311243838389: 3.37548723113504e-231
  -35.411413420693506: 1.652779838762798e-244
  mean: -11.744130457240878
  Credible interval (0.84): -17.915669925622627..-4.071157086251164

  var : w1
  0.22081570934903916: 0.006571617203032474
  0.2427849718438277: 0.006561543895305582
  ...
  0.006023320260298338: 3.37548723113504e-231
  0.00010055461941688916: 1.652779838762798e-244
  mean: 0.18525844047202417
  Credible interval (0.84): 0.05639194312462716..0.2605073855664538

  var : p70
  #f: 0.7762809972955498
  #t: 0.22371900270445083
  mean: 0.22371900270445083
  Credible interval (0.84): 0..1

  var : p75
  #f: 0.9016539339991004
  #t: 0.09834606600090055
  mean: 0.09834606600090055
  Credible interval (0.84): 0..0

  var : breakpoint
  60: 0.0596944817153189
  75: 0.05518884898797974
  ...
  73: 0.014313962251909739
  65: 0.012213988836408039
  mean: 67.37473377658205
  Credible interval (0.84): 53..76


  Check the model

  (w0 -10.317001300317889 w1 0.16553213904510197 breakpoint 66.99666666666667)

  (val: 53 y-this: 0.176 y-bool: 0 y-correct: 0 check: OK     breakpoint: <)
  (val: 57 y-this: 0.293 y-bool: 0 y-correct: 0 check: OK     breakpoint: <)
  (val: 58 y-this: 0.328 y-bool: 0 y-correct: 0 check: OK     breakpoint: <)
  (val: 63 y-this: 0.528 y-bool: 1 y-correct: 0 check: NOT OK breakpoint: <)
  (val: 66 y-this: 0.648 y-bool: 1 y-correct: 1 check: OK     breakpoint: <)
  (val: 67 y-this: 0.684 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 67 y-this: 0.684 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 67 y-this: 0.684 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 68 y-this: 0.719 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 69 y-this: 0.751 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 70 y-this: 0.781 y-bool: 1 y-correct: 0 check: NOT OK breakpoint: >)
  (val: 70 y-this: 0.781 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 70 y-this: 0.781 y-bool: 1 y-correct: 0 check: NOT OK breakpoint: >)
  (val: 70 y-this: 0.781 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 72 y-this: 0.832 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 73 y-this: 0.854 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 75 y-this: 0.891 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 75 y-this: 0.891 y-bool: 1 y-correct: 0 check: NOT OK breakpoint: >)
  (val: 76 y-this: 0.906 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 76 y-this: 0.906 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 78 y-this: 0.931 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 79 y-this: 0.940 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (val: 81 y-this: 0.957 y-bool: 1 y-correct: 1 check: OK     breakpoint: >)
  (num-correct 19 pct 0.8260869565217391)



  This is a port of my WebPPL model logistic_regression_challenger.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define xs '(66 70 69 68 67 72 73 70 57 63 70 78 67 53 67 75 70 81 76 79 75 76 58))
; (define ys '(true false true true true true true true false false false true true false true true true true true true false true false))
(define ys '(1 0 1 1 1 1 1 1 0 0 0 1 1 0 1 1 1 1 1 1 0 1 0))

(define min-xs (apply min xs))
(define max-xs (apply max xs))
(show2 "min xs"  min-xs "min xs" max-xs)

; (define xs-sorted (sort xs <))
; (show "xs-sorted" xs-sorted)

(define data (sort (map (lambda (a b) (list a b)) xs ys) < #:key first))
(show "data" data)

(define (model)
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler 

   (define len (length xs))
   
   (define w0 (normal 0 10))
   (define w1 (normal 0 10))

   ;; Restrict w0 to be negative and w1 positive.
   ;; This makes smaller xs values to be false and large to be true.
   (observe/fail (< w0 0))
   (observe/fail (> w1 0))

   (for ([i len])
     (let* ([x (list-ref xs i)]
            [y (list-ref ys i)]
            [z (+ w0 (* w1 x))]
            [p (/ (+ 1 (exp (- z))))])
       ; (observe (bernoulli p) y) ; slower
       (observe-sample (bernoulli-dist p) y)
       )
     )

   ;; Find the breakpoint when false switches to true
   ;; Note: this use the original data xs and ys.
   (define breakpoint (uniform-draw (range min-xs (add1 max-xs))))
   
   (for ([i len])
     (if (< (list-ref xs i) breakpoint)
         (= (list-ref ys i) 0)
         (= (list-ref ys i) 1)         
         ))
  
   ;; What is the probability that the values of 70 and 75 are false?
   (define p70 (eq? false (flip (/ (+ 1 (exp (- (* (+ w0 (* w1 70))))))))))
   (define p75 (eq? false (flip (/ (+ 1 (exp (- (* (+ w0 (* w1 75))))))))))
  
   (list w0
         w1
         p70
         p75
         breakpoint
         )

   )
  )


(show-marginals (model)
                (list "w0"
                      "w1"
                      "p70"
                      "p75"
                      "breakpoint"
                      "xs post"
                      )
                #:num-samples 10000
                #:truncate-output 2
                ; #:skip-marginals? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


(displayln "\nCheck the model")
(define *samples* (make-samples (model) 300))
(define w0 (avg (map (lambda (v) (list-ref v 0)) *samples*)))
(define w1 (avg (map (lambda (v) (list-ref v 1)) *samples*)))
(define breakpoint (avg (map (lambda (v) (list-ref v 4)) *samples*)))
(show2 "w0" w0 "w1" w1 "breakpoint" (* 1.0 breakpoint))
(newline)
(define num-correct 0)
(for ([i (length data)])
  (let* ([d (list-ref data i)]
         [x (list-ref d 0)]
         [y (/ (+ 1 (exp (- (* (+ w0 (* w1 x)))))))]
         [y-bool (if (< y 0.5) 0 1)]         
         [y-correct (list-ref d 1)])
    (show2 "x:" x
           "y:" (~r y #:precision '(= 3))
           "y-bool:" y-bool
           "y-correct:" y-correct
           "check:" (if (= y-bool y-correct) "OK    "
                                             "NOT OK")
           "breakpoint:" (if (< x breakpoint) "<" ">" ))
    (when (= y-bool y-correct) (set! num-correct (add1 num-correct)))
    )
  )
(newline)
(show2 "num-correct" num-correct "pct" (* 1.0 (/ num-correct (length data))))
(newline)
