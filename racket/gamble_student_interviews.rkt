#| 

  Student interviews in Racket.Gamble 

  From SPPL model student-interviews.pynb

  Here is the output from the SPPL model of gpa_prior and gpa_posterior for gpa_student(1):

        gpa                   gpa_prior (cumsum < gpa)  gpa_posterior (cumsum < gpa)
        -----------------------------------------------------------------------------
        0.0                   0.0                    0.0
        0.21052631578947367   2.9321229365490116e-08 3.938526167438616e-08
        0.42105263157894735   3.399923850680484e-06  4.566892092513705e-06
        0.631578947368421     5.235987400911553e-05  7.03315441989632e-05
        0.8421052631578947    0.0003515966799506029  0.0004722764885157954
        1.0526315789473684    0.0014935106089171875  0.0020061336928425886
        1.263157894736842     0.004734542792041406   0.0063595971522462345
        1.4736842105263157    0.012227795632612991   0.01642478644698973
        1.6842105263157894    0.027098290512422067   0.03639933542542442
        1.894736842105263     0.05325350529777253    0.07153189980839067
        2.1052631578947367    0.09485332168120815    0.12741017262720986
        2.3157894736842106    0.15542018995700252    0.20876562761532186
        2.526315789473684     0.23665218641802727    0.31787917797401755
        2.7368421052631575    0.3371086961787463     0.45281574132555596
        2.9473684210526314    0.45107070187375614    0.6058933411522036
        3.1578947368421053    0.5680350963535515     0.7630038505974432
        3.3684210526315788    0.6734850651989842     0.9046477961757059
        3.578947368421052     0.7517864026032522     0.983997710206192
        3.789473684210526     0.7922926319251767     0.9970552758034202
        4.0                   0.8                    0.9995398186972839

        
  The mean value of the prior is about: 3.050505050505051
  The mean value of the posterior is about: 2.8080808080808084

  (I am not sure how to get an exact value of the mean in SPPL. 
   For continous distributions, SPPL requires that one work with 
   intervals.)


var : num_recruiters
31: 0.34000000000009595
32: 0.2521400000000578
33: 0.15313000000003324
34: 0.1008500000000225
35: 0.07149000000001866
36: 0.041250000000011236
37: 0.02288000000000596
38: 0.009600000000002433
39: 0.005700000000001447
40: 0.002960000000000756
mean: 32.62988000000814

var : student_perfect_prior
#f: 0.7952900000000338
#t: 0.20471000000004555
mean: 0.20471000000004555

var : student_gpa_prior
4: 0.20471000000004555
2.3466406923927656: 0.0007200000000001826
2.900713778243574: 0.0006000000000001521
2.664063633788913: 0.0005800000000001471
...
mean: 3.0437026290286986

var : student_perfect 0
#f: 0.9996199999999501
#t: 0.00038000000000009635
mean: 0.00038000000000009635

var : student_gpa 0
1.7142515718027522: 0.001030000000000261
2.224734650076054: 0.0008800000000002232
2.2393104529216763: 0.000880000000000223
...
mean: 2.727334194465453

var : student_interviews 0
13: 0.1419300000000303
14: 0.13238000000002784
11: 0.11843000000002488
12: 0.11268000000002451
15: 0.09819000000002201
10: 0.09523000000002141
16: 0.0812800000000196
9: 0.060050000000016514
17: 0.04867000000001334
8: 0.03117000000000816
18: 0.023890000000006264
7: 0.01602000000000408
19: 0.01596000000000409
20: 0.009220000000002332
6: 0.005250000000001334
21: 0.003940000000001005
22: 0.002650000000000673
5: 0.002100000000000534
4: 0.00034000000000008627
25: 0.00020000000000005075
24: 0.0001600000000000406
23: 0.00012000000000003046
29: 5.000000000001268e-5
30: 5.000000000001268e-5
26: 4.000000000001015e-5
mean: 12.92592000000296

var : student_offers 0
1: 0.9999999999999499
mean: 0.9999999999999499

var : student_perfect 1
#t: 0.8261700000000484
#f: 0.17383000000003596
mean: 0.8261700000000484

var : student_gpa 1
4.0: 0.8261700000000484
3.728262711108712: 0.0007200000000001826
3.799132142706011: 0.0006100000000001546
...
mean: 3.8234054182378956

var : student_interviews 1
30: 0.15251000000003312
29: 0.1521600000000334
28: 0.1392300000000311
31: 0.10890000000002407
27: 0.0870900000000205
32: 0.06341000000001745
26: 0.04534000000001229
33: 0.035920000000009514
21: 0.029160000000007798
20: 0.027500000000007317
22: 0.026410000000006994
25: 0.020590000000005347
23: 0.01994000000000522
19: 0.019750000000005194
24: 0.01839000000000475
34: 0.01736000000000444
18: 0.0136500000000035
17: 0.007250000000001822
35: 0.007240000000001837
36: 0.004330000000001104
16: 0.0023400000000005977
37: 0.0006100000000001547
15: 0.0005700000000001443
38: 0.00026000000000006595
39: 5.000000000001268e-5
14: 4.000000000001015e-5
mean: 27.989810000006628

var : student_offers 1
13: 0.9999999999999499
mean: 12.99999999999935


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

; Note: The SPPL model has beta(a=7,b=3,scale=4),
; but this form is not directly supported in Gamble (or in WebPPL),
(define (beta_scale a b scale) (* (beta a b) scale))


(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define num_students 2)

   ;; Did student get a perfect score?
   (defmem (student_perfect s) (flip 0.2))
   
   ;; GPA.
   (defmem (student_gpa s)
     (if (student_perfect s)
         4.0
         (beta_scale 7 3 4)))
    
   ;; Number of recruiters, betwen 10 and 40
   (define num_recruiters (poisson 25))
   (observe/fail (and (>= num_recruiters 10) (<= num_recruiters 40))) ;; truncated  
   ; (observe/fail (and (>= 20 num_recruiters) (<= num_recruiters 40))) ; Alternative version
   
   ; Using clip-distx does not work on poisson-dist: "expected: real-dist?"
   ; (define num_recruiters (sample (clip-distx (poisson-dist 25) 10 40)))
   
   
   ;; Number of interviews
   (defmem (student_interviews s)
     (cond
       [(= (student_gpa s) 4) (binomial num_recruiters 0.9)]
       [(and (< 3.5 (student_gpa s)) (< (student_gpa s) 4)) (binomial num_recruiters 0.6)]
       [else (binomial num_recruiters 0.5)]))

   ;; How many offers did a student got?
   (defmem (student_offers s)
     (if (= (student_interviews s) 0)
         0
         (binomial (student_interviews s) 0.4)))
   
   ;; Observations
   
   ;; Student 0
   (observe/fail (and (= (student_offers 0) 1) (> num_recruiters 30)))
   ;; (observe/fail (= (student_offers 0) 1))
   
   ;; Student 1
   ;; (observe/fail (student_perfect 1))
   (observe/fail (= (student_offers 1) 13))
   ; (observe-sample (dist-unit (student_offers 1)) 13)

   ;; (observe/fail (= student_interviews 1) 20))
   
   ;; Num recruiters
   ;; (observe/fail (> num_recruiters 30))
   
   ;; Priors
   (define student_perfect_prior (flip 0.2))
   (define student_gpa_prior (if (not student_perfect_prior) (* (beta 7 3) 4) 4))
   
    
    (list num_recruiters
          student_perfect_prior
          student_gpa_prior
          
          (student_perfect 0)
          (student_gpa 0)
          (student_interviews 0)
          (student_offers 0)
          
          (student_perfect 1)
          (student_gpa 1)
          (student_interviews 1)
          (student_offers 1)
          )
    
   )
)

(show-marginals (model)
              (list "num_recruiters"
                    "student_perfect_prior"
                    "student_gpa_prior"
                    
                    "student_perfect 0"
                    "student_gpa 0"
                    "student_interviews 0"
                    "student_offers 0"
                    
                    "student_perfect 1"
                    "student_gpa 1"
                    "student_interviews 1"
                    "student_offers 1"
                    )
                    #:num-samples 100000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


